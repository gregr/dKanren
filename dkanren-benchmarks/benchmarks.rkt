#lang racket/base

(require
  (rename-in "closure-encoding.rkt"
             (term? term-closure-encoded?)
             (eval-term eval-term-closure-encoded)
             (initial-env initial-env-closure-encoded))
  (rename-in "raw.rkt"
             (term? term-raw?)
             (eval-term eval-term-raw)
             (initial-env initial-env-raw))
  racket/list
  racket/match
  )

(define problem-iterations 100)
(define problem-size 10)
;; Use this size to differentiate immediate and runtime scheme eval, but
;; remember to turn off the slow evaluators!
;(define problem-size 10000)

(define (run/scheme-eval term) (eval term))
(define (run/raw-eval term) (eval-term-raw term initial-env-raw))
(define (run/closure-eval term) (eval-term-closure-encoded term initial-env-closure-encoded))

(define (run/scheme-eval-eval term) (run/eval-eval run/scheme-eval term))
(define (run/closure-eval-eval term) (run/eval-eval run/closure-eval term))
(define (run/raw-eval-eval term) (run/eval-eval run/raw-eval term))

(define (run/eval-eval run/eval term)
  (run/eval
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
                       (`(lambda ,params ,body)
                        `(,closure-tag (lambda ,params ,body) ,env))
                       (`(letrec ((,p-name (lambda ,params ,body)))
                           ,letrec-body)
                        (eval-term
                          letrec-body
                          `((,p-name . (rec . (lambda ,params ,body)))
                            . ,env))))))))))

           (let ((program ',term)) (eval-term program initial-env)))))))

(define ex-append
  `(letrec ((append
              (lambda (xs ys)
                (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
     (list . ,(make-list problem-iterations
                         `(append ',(range problem-size) '())))))
(define ex-reverse-quadratic
  `(letrec ((append
              (lambda (xs ys)
                (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
     (letrec ((reverse
                (lambda (xs)
                  (if (null? xs)
                    '()
                    (append (reverse (cdr xs)) (list (car xs)))))))
       (list . ,(make-list problem-iterations
                           `(reverse ',(range problem-size)))))))

(define (benchmark)
  (let loop-prog ((programs `((append ,ex-append)
                              (reverse-quadratic ,ex-reverse-quadratic)
                              )))
    (when (pair? programs)
      (let ((program (car programs)) (programs (cdr programs)))
        (newline)
        (displayln `(program: ,(car program)))
        (let loop-eval ((runners `((scheme-eval-static
                                     ,(eval `(lambda (,(gensym "unused"))
                                               ,(cadr program))))
                                   (scheme-eval-runtime ,run/scheme-eval)
                                   (closure-eval ,run/closure-eval)
                                   (raw-eval ,run/raw-eval)
                                   ;; TODO: this needs to require racket/match
                                   ;(scheme-eval-eval ,run/scheme-eval-eval)
                                   (closure-eval-eval ,run/closure-eval-eval)
                                   (raw-eval-eval ,run/raw-eval-eval)
                                   )))
          (if (null? runners) (loop-prog programs)
            (let ((runner (car runners)) (runners (cdr runners)))
              (collect-garbage 'major)
              (displayln `(evaluator: ,(car runner)))
              (time (void ((cadr runner) (cadr program))))
              ;(time (displayln ((cadr runner) (cadr program))))
              (loop-eval runners))))))))

(benchmark)

; TODO: port these to Chez Scheme

; deterministic evaluation benchmark ideas to measure sources of overhead
; across programs: append, reverse, map, fold, mini interpreter, remove-foo, etc.
; across program implementations: tailcall, w/ fold, etc.
; across runtimes:
;   scheme, mk-only, mixed
; across interpreter architectures:
;   static (scheme only)
;   eval at runtime (scheme only)
;   ahead-of-time compiled (dkanren only)
;   closure encoding (mk would need to support procedure values)
;   de bruin encoding (with and without integer support)
;   raw interpretation
;   original relational interpreter(s) (mk only)
