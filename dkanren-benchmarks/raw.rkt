#lang racket/base
(provide
  term?
  eval-term
  initial-env
  )

(require
  racket/match
  )

(module+ test
  (require
    rackunit
    ))

(define closure-tag (gensym "#%closure"))
(define (closure-tag? x) (eq? x closure-tag))
(define prim-tag (gensym "#%primitive"))
(define (prim-tag? x) (eq? x prim-tag))

(define (applicable-tag? v)
  (or (equal? closure-tag v) (equal? prim-tag v)))
(define (quotable? v)
  (match v
    ((? symbol?) (not (applicable-tag? v)))
    (`(,a . ,d) (and (quotable? a) (quotable? d)))
    (_ #t)))

(define empty-env '())
(define initial-env `((val . (cons . (,prim-tag . cons)))
                      (val . (car . (,prim-tag . car)))
                      (val . (cdr . (,prim-tag . cdr)))
                      (val . (null? . (,prim-tag . null?)))
                      (val . (pair? . (,prim-tag . pair?)))
                      (val . (symbol? . (,prim-tag . symbol?)))
                      (val . (number? . (,prim-tag . number?)))
                      (val . (not . (,prim-tag . not)))
                      (val . (equal? . (,prim-tag . equal?)))
                      (val . (list . (,closure-tag (lambda x x) ,empty-env)))
                      . ,empty-env))
(define (in-env? env sym)
  (match env
    ('() #f)
    (`((val . (,a . ,_)) . ,d) (or (equal? a sym) (in-env? d sym)))
    (`((rec . ,binding*) . ,d) (in-env-rec? binding* d sym))))
(define (in-env-rec? binding* env sym)
  (match binding*
    ('() (in-env? env sym))
    (`((,a . ,_) . ,d) (or (equal? a sym) (in-env-rec? d env sym)))))
(define (extend-env* params args env)
  (match `(,params . ,args)
    (`(() . ()) env)
    (`((,x . ,dx*) . (,a . ,da*))
      (extend-env* dx* da* `((val . (,x . ,a)) . ,env)))))
(define (lookup env sym)
  (match env
    (`((val . (,y . ,b)) . ,rest) (if (equal? sym y) b (lookup rest sym)))
    (`((rec . ,binding*) . ,rest) (lookup-rec binding* rest sym env))))
(define (lookup-rec binding* env sym renv)
  (match binding*
    ('() (lookup env sym))
    (`((,p-name ,lam-expr) . ,binding*)
      (if (equal? sym p-name)
        `(,closure-tag ,lam-expr ,renv)
        (lookup-rec binding* env sym renv)))))

(define (not-in-params? ps sym)
  (match ps
    ('() #t)
    (`(,a . ,d)
      (and (not (equal? a sym)) (not-in-params? d sym)))))
(define (param-list? x)
  (match x
    ('() #t)
    (`(,(? symbol? a) . ,d)
      (and (param-list? d) (not-in-params? d a)))
    (_ #f)))
(define (params? x)
  (match x
    ((? param-list?) #t)
    (x (symbol? x))))
(define (bindings? b*)
  (match b*
    ('() #t)
    (`((,p ,v) . ,b*) (bindings? b*))
    (_ #f)))
(define (split-bindings b*)
  (match b*
    ('() (values '() '()))
    (`((,param ,val) . ,b*)
      (let-values (((ps vs) (split-bindings b*)))
        (values (cons param ps) (cons val vs))))))

(define (pattern-var? b* vname ps)
  (match b*
    ('() `(,vname . ,ps))
    (`(,name . ,b*) (if (eq? name vname) ps (pattern-var? b* vname ps)))))
(define (pattern-qq? qqpattern ps env)
  (match qqpattern
    (`(,'unquote ,pat) (pattern? pat ps env))
    (`(,a . ,d) (let ((ps (pattern-qq? a ps env)))
                  (and ps (pattern-qq? d ps env))))
    ((? quotable?) ps)))
(define (pattern-or? pattern* ps env)
  (match pattern*
    ('() ps)
    (`(,pattern . ,pattern*)
      (let ((ps0 (pattern? pattern ps env)))
        (and (equal? ps0 (pattern-or? pattern* ps env)) ps0)))))
(define (pattern*? pattern* ps env)
  (match pattern*
    ('() ps)
    (`(,pattern . ,pattern*)
      (let ((ps (pattern? pattern ps env)))
        (and ps (pattern*? pattern* ps env))))))
(define (pattern? pattern ps env)
  (match pattern
    (`(quote ,(? quotable?)) ps)
    (`(quasiquote ,qqpat) (pattern-qq? qqpat ps env))
    (`(not . ,pat*) (pattern*? pat* ps env))
    (`(and . ,pat*) (pattern*? pat* ps env))
    (`(or . ,pat*) (pattern-or? pat* ps env))
    (`(? ,predicate . ,pat*)
      (and (term? predicate env) (pattern*? pat* ps env)))
    ('_ ps)
    ((? symbol? vname) (pattern-var? ps vname ps))
    ((? quotable?) ps)))
(define (match-clauses? pt* env)
  (match pt*
    ('() #t)
    (`((,pat ,rhs) . ,pt*)
      (let ((ps (pattern? pat '() env)))
        (and
          ps (term? rhs (extend-env* ps ps env)) (match-clauses? pt* env))))))

(define (term-qq? qqterm env)
  (match qqterm
    (`(,'unquote ,term) (term? term env))
    (`(,a . ,d) (and (term-qq? a env) (term-qq? d env)))
    (datum (quotable? datum))))
(define (term? term env)
  (letrec ((term1? (lambda (v) (term? v env)))
           (terms? (lambda (ts env)
                     (match ts
                       ('() #t)
                       (`(,t . ,ts) (and (term? t env) (terms? ts env))))))
           (binding-lambdas?
             (lambda (binding* env)
               (match binding*
                 ('() #t)
                 (`((,_ ,(and `(lambda ,_ ,_) t)) . ,b*)
                   (and (term? t env) (binding-lambdas? b* env)))))))
    (match term
      (#t #t)
      (#f #t)
      ((? number?) #t)
      ((and (? symbol? sym)) (in-env? env sym))
      (`(,(? term1?) . ,rands) (terms? rands env))
      (`(quote ,datum) (quotable? datum))
      (`(quasiquote ,qqterm) (term-qq? qqterm env))
      (`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
      (`(lambda ,params ,body)
        (and (params? params)
             (let ((res (match params
                          ((not (? symbol? params))
                           (extend-env* params params env))
                          (sym `((val . (,sym . ,sym)) . ,env)))))
               (term? body res))))
      (`(let ,binding* ,let-body)
        (and (bindings? binding*)
             (let-values (((ps vs) (split-bindings binding*)))
               (and (terms? vs env)
                    (term? let-body (extend-env* ps ps env))))))
      (`(letrec ,binding* ,letrec-body)
        (let ((res `((rec . ,binding*) . ,env)))
          (and (binding-lambdas? binding* res) (term? letrec-body res))))
      (`(and . ,t*) (terms? t* env))
      (`(or . ,t*) (terms? t* env))
      (`(match ,s . ,pt*) (and (term1? s) (match-clauses? pt* env)))
      (_ #f))))

(define (eval-pattern-literal literal penv v) (and (equal? literal v) penv))
(define (eval-pattern-var b* vname penv v)
  (match b*
    ('() `((,vname ,v) . ,penv))
    (`((,name ,x) . ,b*)
      (if (eq? name vname)
        (and (equal? x v) penv)
        (eval-pattern-var b* vname penv v)))))
(define (eval-pattern-qq qqpattern penv v env)
  (match qqpattern
    (`(,'unquote ,pat) (eval-pattern pat penv v env))
    (`(,a . ,d)
      (and (pair? v)
           (let ((penv (eval-pattern-qq a penv (car v) env)))
             (and penv (eval-pattern-qq d penv (cdr v) env)))))
    ((? quotable? datum) (eval-pattern-literal datum penv v))))
(define (eval-pattern-or pattern* penv v env)
  (match pattern*
    ('() #f)
    (`(,pattern . ,pattern*)
      (let ((penv0 (eval-pattern pattern penv v env)))
        (or penv0 (eval-pattern-or pattern* penv v env))))))
(define (eval-pattern* pattern* penv v env)
  (match pattern*
    ('() penv)
    (`(,pattern . ,pattern*)
      (let ((penv (eval-pattern pattern penv v env)))
        (and penv (eval-pattern* pattern* penv v env))))))
(define (eval-pattern pattern penv v env)
  (match pattern
    (`(quote ,(? quotable? datum)) (eval-pattern-literal datum penv v))
    (`(quasiquote ,qqpat) (eval-pattern-qq qqpat penv v env))
    (`(not . ,pat*) (and (not (eval-pattern* pat* penv v env)) penv))
    (`(and . ,pat*) (eval-pattern* pat* penv v env))
    (`(or . ,pat*) (eval-pattern-or pat* penv v env))
    (`(? ,predicate . ,pat*)
      (and (eval-application (eval-term predicate env) (list v))
           (eval-pattern* pat* penv v env)))
    ('_ penv)
    ((? symbol? vname) (eval-pattern-var penv vname penv v))
    ((? quotable? datum) (eval-pattern-literal datum penv v))))
(define (eval-match pt* v env)
  (match pt*
    (`((,pat ,rhs) . ,pt*)
      (let ((penv (eval-pattern pat '() v env)))
        (if penv
          (let-values (((ps vs) (split-bindings penv)))
            (eval-term rhs (extend-env* ps vs env)))
          (eval-match pt* v env))))))

(define (eval-prim prim-id args)
  (match `(,prim-id . ,args)
    (`(cons ,a ,d) `(,a . ,d))
    (`(car (,(and (not (? applicable-tag?)) a) . ,d)) a)
    (`(cdr (,(and (not (? applicable-tag?)) a) . ,d)) d)
    (`(null? ,v) (match v ('() #t) (_ #f)))
    (`(pair? ,v) (match v (`(,(not (? applicable-tag?)) . ,_) #t) (_ #f)))
    (`(symbol? ,v) (symbol? v))
    (`(number? ,v) (number? v))
    (`(not ,v) (match v (#f #t) (_ #f)))
    (`(equal? ,v1 ,v2) (equal? v1 v2))))
(define (eval-qq qqterm env)
  (match qqterm
    (`(,'unquote ,term) (eval-term term env))
    (`(,a . ,d) `(,(eval-qq a env) . ,(eval-qq d env)))
    ((? quotable? datum) datum)))
(define (eval-and t* env)
  (match t*
    ('() #t)
    (`(,t) (eval-term t env))
    (`(,t . ,t*) (if (eval-term t env) (eval-and t* env) #f))))
(define (eval-or t* env)
  (match t*
    ('() #f)
    (`(,t) (eval-term t env))
    (`(,t . ,t*) (let ((condition (eval-term t env)))
                   (if condition condition (eval-or t* env))))))
(define (eval-application proc a*)
  (match proc
    (`(,(? prim-tag?) . ,prim-id) (eval-prim prim-id a*))
    (`(,(? closure-tag?) (lambda ,x ,body) ,env^)
      (let ((res (match x
                   ((and (not (? symbol?)) params)
                    (extend-env* params a* env^))
                   (sym `((val . (,sym . ,a*)) . ,env^)))))
        (eval-term body res)))))
(define (eval-term-list terms env)
  (match terms
    ('() '())
    (`(,term . ,terms)
      `(,(eval-term term env) . ,(eval-term-list terms env)))))
(define (eval-term term env)
  (let ((bound? (lambda (sym) (in-env? env sym))))
    (match term
      (#t #t)
      (#f #f)
      ((? number? num) num)
      ((? symbol? sym) (lookup env sym))
      ((and `(,op . ,_) operation)
       (match operation
         (`(,(or (? bound?) (not (? symbol?))) . ,rands)
           (let ((op (eval-term op env))
                 (a* (eval-term-list rands env)))
             (eval-application op a*)))
         (`(quote ,(? quotable? datum)) datum)
         (`(quasiquote ,qqterm) (eval-qq qqterm env))
         (`(if ,condition ,alt-true ,alt-false)
           (if (eval-term condition env)
             (eval-term alt-true env)
             (eval-term alt-false env)))
         (`(lambda ,params ,body)
          `(,closure-tag (lambda ,params ,body) ,env))
         (`(let ,binding* ,let-body)
           (let-values (((ps vs) (split-bindings binding*)))
             (eval-term let-body
                        (extend-env* ps (eval-term-list vs env) env))))
         (`(letrec ,binding* ,letrec-body)
          (eval-term letrec-body `((rec . ,binding*) . ,env)))
         (`(and . ,t*) (eval-and t* env))
         (`(or . ,t*) (eval-or t* env))
         (`(match ,scrutinee . ,pt*)
           (eval-match pt* (eval-term scrutinee env) env)))))))

(module+ test
  (check-equal? (eval-term 3 initial-env) 3)
  (check-equal? (eval-term '3 initial-env) 3)
  (check-equal? (eval-term ''x initial-env) 'x)
  (check-equal? (eval-term ''(1 (2) 3) initial-env) '(1 (2) 3))
  (check-equal? (eval-term '(car '(1 (2) 3)) initial-env) 1)
  (check-equal? (eval-term '(cdr '(1 (2) 3)) initial-env) '((2) 3))
  (check-equal? (eval-term '(cons 'x 4) initial-env) '(x . 4))
  (check-equal? (eval-term '(null? '()) initial-env) #t)
  (check-equal? (eval-term '(null? '(0)) initial-env) #f)
  (check-equal? (eval-term '(list 5 6) initial-env) '(5 6))
  (check-equal? (eval-term '(and #f 9 10) initial-env) #f)
  (check-equal? (eval-term '(and 8 9 10) initial-env) 10)
  (check-equal? (eval-term '(or #f 11 12) initial-env) 11)
  (check-equal? (eval-term '(let ((p (cons 8 9))) (cdr p)) initial-env) 9)

  (define ex-append
    '(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       (list (append '() '()) (append '(foo) '(bar)) (append '(1 2) '(3 4)))) )
  (define ex-append-answer '(() (foo bar) (1 2 3 4)))
  (check-true (term? ex-append initial-env))
  (check-equal? (eval-term ex-append initial-env) ex-append-answer)
  (check-equal? (eval-term '`(1 ,(car `(,(cdr '(b 2)) 3)) ,'a) initial-env)
    '(1 (2) a))

  (check-equal?
    (eval-term
      '(match '(1 (b 2))
         (`(1 (a ,x)) 3)
         (`(1 (b ,x)) x)
         (_ 4))
      initial-env)
    2)
  (check-equal?
    (eval-term
      '(match '(1 1 2)
         (`(,a ,b ,a) `(first ,a ,b))
         (`(,a ,a ,b) `(second ,a ,b))
         (_ 4))
      initial-env)
    '(second 1 2))

  (define ex-match
    '(match '(1 2 1)
       (`(,a ,b ,a) `(first ,a ,b))
       (`(,a ,a ,b) `(second ,a ,b))
       (_ 4)))
  (check-true (term? ex-match initial-env))
  (check-equal?
    (eval-term ex-match initial-env)
    '(first 1 2))

  (define ex-eval-expr
    '(letrec
       ((eval-expr
          (lambda (expr env)
            (match expr
              (`(quote ,datum) datum)
              (`(lambda (,(? symbol? x)) ,body)
                (lambda (a)
                  (eval-expr body (lambda (y)
                                    (if (equal? y x) a (env y))))))
              ((? symbol? x) (env x))
              (`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env)))
              (`(,rator ,rand) ((eval-expr rator env)
                                (eval-expr rand env)))))))
       (list
         (eval-expr '((lambda (y) y) 'g1) 'initial-env)
         (eval-expr '(((lambda (z) z) (lambda (v) v)) 'g2) 'initial-env)
         (eval-expr '(((lambda (a) (a a)) (lambda (b) b)) 'g3) 'initial-env)
         (eval-expr '(((lambda (c) (lambda (d) c)) 'g4) 'g5) 'initial-env)
         (eval-expr '(((lambda (f) (lambda (v1) (f (f v1)))) (lambda (e) e)) 'g6) 'initial-env)
         (eval-expr '((lambda (g) ((g g) g)) (lambda (i) (lambda (j) 'g7))) 'initial-env))))
  (check-true (term? ex-eval-expr initial-env))
  (check-equal?
    (eval-term ex-eval-expr initial-env)
    '(g1 g2 g3 g4 g6 g7))

  ; the goal is to support something like this interpreter
  (define ex-eval-complex
    `(let ((closure-tag ',(gensym "#%closure"))
           (prim-tag ',(gensym "#%primitive"))
           (empty-env '()))
       (let ((initial-env
               `((cons . (val . (,prim-tag . cons)))
                 (car . (val . (,prim-tag . car)))
                 (cdr . (val . (,prim-tag . cdr)))
                 (null? . (val . (,prim-tag . null?)))
                 (pair? . (val . (,prim-tag . pair?)))
                 (symbol? . (val . (,prim-tag . symbol?)))
                 (not . (val . (,prim-tag . not)))
                 (equal? . (val . (,prim-tag . equal?)))
                 (list . (val . (,closure-tag (lambda x x) ,empty-env)))
                 . ,empty-env))
             (closure-tag? (lambda (v) (equal? v closure-tag)))
             (prim-tag? (lambda (v) (equal? v prim-tag))))
         (letrec
           ((applicable-tag? (lambda (v) (or (closure-tag? v) (prim-tag? v))))
            (quotable? (lambda (v)
                         (match v
                           ((? symbol?) (not (applicable-tag? v)))
                           (`(,a . ,d) (and (quotable? a) (quotable? d)))
                           (_ #t))))
            (not-in-params? (lambda (ps sym)
                              (match ps
                                ('() #t)
                                (`(,a . ,d)
                                  (and (not (equal? a sym))
                                       (not-in-params? d sym))))))
            (param-list? (lambda (x)
                           (match x
                             ('() #t)
                             (`(,(? symbol? a) . ,d)
                               (and (param-list? d) (not-in-params? d a)))
                             (_ #f))))
            (params? (lambda (x)
                       (match x
                         ((? param-list?) #t)
                         (x (symbol? x)))))
            (in-env? (lambda (env sym)
                       (match env
                         ('() #f)
                         (`((,a . ,_). ,d)
                           (or (equal? a sym) (in-env? d sym))))))
            (extend-env*
              (lambda (params args env)
                (match `(,params . ,args)
                  (`(() . ()) env)
                  (`((,x . ,dx*) . (,a . ,da*))
                    (extend-env* dx* da* `((,x . (val . ,a)) . ,env))))))
            (lookup
              (lambda (env sym)
                (match env
                  (`((,y . ,b) . ,rest)
                    (if (equal? sym y)
                      (match b
                        (`(val . ,v) v)
                        (`(rec . ,lam-expr) `(,closure-tag ,lam-expr ,env)))
                      (lookup rest sym))))))
            (term?
              (lambda (term env)
                (letrec
                  ((term1? (lambda (v) (term? v env)))
                   (terms? (lambda (ts env)
                             (match ts
                               ('() #t)
                               (`(,t . ,ts)
                                 (and (term? t env) (terms? ts env)))))))
                  (match term
                    (#t #t)
                    (#f #t)
                    ((? number?) #t)
                    ((and (? symbol? sym)) (in-env? env sym))
                    (`(,(? term1?) . ,rands) (terms? rands env))
                    (`(quote ,datum) (quotable? datum))
                    (`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
                    (`(lambda ,params ,body)
                      (and (params? params)
                           (let ((res
                                   (match params
                                     ((and (not (? symbol?)) params)
                                      (extend-env* params params env))
                                     (sym `((,sym . (val . ,sym)) . ,env)))))
                             (term? body res))))
                    (`(letrec
                        ((,p-name ,(and `(lambda ,params ,body) lam-expr)))
                        ,letrec-body)
                      (and (params? params)
                           (let ((res `((,p-name
                                          . (rec . (lambda ,params ,body)))
                                        . ,env)))
                             (and (term? lam-expr res)
                                  (term? letrec-body res)))))
                    (_ #f)))))
            (eval-prim
              (lambda (prim-id args)
                (match `(,prim-id . ,args)
                  (`(cons ,a ,d) `(,a . ,d))
                  (`(car (,(and (not (? applicable-tag?)) a) . ,d)) a)
                  (`(cdr (,(and (not (? applicable-tag?)) a) . ,d)) d)
                  (`(null? ,v) (match v ('() #t) (_ #f)))
                  (`(pair? ,v) (match v
                                 (`(,(not (? applicable-tag?)) . ,_) #t)
                                 (_ #f)))
                  (`(symbol? ,v) (symbol? v))
                  (`(number? ,v) (number? v))
                  (`(not ,v) (match v (#f #t) (_ #f)))
                  (`(equal? ,v1 ,v2) (equal? v1 v2)))))
            (eval-term-list
              (lambda (terms env)
                (match terms
                  ('() '())
                  (`(,term . ,terms)
                    `(,(eval-term term env) . ,(eval-term-list terms env))))))
            (eval-term
              (lambda (term env)
                (let ((bound? (lambda (sym) (in-env? env sym)))
                      (term1? (lambda (v) (term? v env))))
                  (match term
                    (#t #t)
                    (#f #f)
                    ((? number? num) num)
                    (`(,(and 'quote (not (? bound?))) ,(? quotable? datum))
                      datum)
                    ((? symbol? sym) (lookup env sym))
                    ((and `(,op . ,_) operation)
                     (match operation
                       (`(,(or (? bound?) (not (? symbol?)))
                           . ,rands)
                         (let ((op (eval-term op env))
                               (a* (eval-term-list rands env)))
                           (match op
                             (`(,(? prim-tag?) . ,prim-id)
                               (eval-prim prim-id a*))
                             (`(,(? closure-tag?) (lambda ,x ,body) ,env^)
                               (let ((res (match x
                                            ((and (not (? symbol?)) params)
                                             (extend-env* params a* env^))
                                            (sym `((,sym . (val . ,a*))
                                                   . ,env^)))))
                                 (eval-term body res))))))
                       (`(if ,condition ,alt-true ,alt-false)
                         (if (eval-term condition env)
                           (eval-term alt-true env)
                           (eval-term alt-false env)))
                       ((? term1? `(lambda ,params ,body))
                        `(,closure-tag (lambda ,params ,body) ,env))
                       ((? term1? `(letrec ((,p-name (lambda ,params ,body)))
                                     ,letrec-body))
                        (eval-term
                          letrec-body
                          `((,p-name . (rec . (lambda ,params ,body)))
                            . ,env))))))))))

        (let ((program ',ex-append))
          (and (term? program initial-env)
               (eval-term program initial-env)))))))
  (check-true (term? ex-eval-complex initial-env))
  (check-equal? (eval-term ex-eval-complex initial-env) ex-append-answer)
  )
