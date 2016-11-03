#lang racket/base
(provide

  )

(require
  racket/match
  )

(module+ test
  (require
    rackunit
    ))

; match pattern compiler
; patterns: _, var, literal, ', `, ?, not, and, or
; ideally, minimize let-bound/fresh vars
; compile pattern for input v:
; _, var: just use (or not use) v
; 'literal or literal: (equal? v literal)
; `: enter quasiquote state
; quasiquoted (P . Q): (and (pair? v) let Pv = recurse P/(car v), let Qv = recurse Q/(cdr v))
; ? p P: (and (p v) (recurse P))
; and: conj*
; or: disj*
; not:
;   (not (not pat)) = pat
;   (not (and P Q)) = (or (not P) (not Q))
;   (not (or P Q)) = (and (not P) (not Q))
;
; implicit pattern negations carried forward to subsequent cases
; translate the same output to both direct and goal interpretations
;   direct can rearrange disjunctions to group algebraic tree nodes for fewer comparisons
;   goals can't easily do this without messing up scheduling priorities
;
; match/r
;
; optional match result-domain annotations
;   type or finite domain: support fast, conservative domain constraints
;     domain = _ (anything), ([quasi]quoted) literal, infinite set (i.e. number, symbol), list (union) of domains
;   last-of-this-value markers: if you have this value, it's the last case where it's possible, so commit to it


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
(define (let->lambda term)
  (match term
    (`(let ,binding* ,let-body)
      (let-values (((ps vs) (split-bindings binding*)))
        `((lambda ,ps ,let-body) . ,vs)))))

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
           (binding-terms? (lambda (binding* env)
                             (match binding*
                               ('() #t)
                               (`((,_ ,(and `(lambda ,_ ,_) t)) . ,b*)
                                 (and (term? t env) (binding-terms? b* env)))))))
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
      (`(let ,b* ,_) (and (bindings? b*) (term? (let->lambda term) env)))
      (`(letrec ,binding* ,letrec-body)
        (let ((res `((rec . ,binding*) . ,env)))
          (and (binding-terms? binding* res) (term? letrec-body res))))
      (_ #f))))

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
(define (eval-term-list terms env)
  (match terms
    ('() '())
    (`(,term . ,terms)
      `(,(eval-term term env) . ,(eval-term-list terms env)))))
(define (eval-term term env)
  (let ((bound? (lambda (sym) (in-env? env sym)))
        (term1? (lambda (v) (term? v env))))
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
             (match op
               (`(,(? prim-tag?) . ,prim-id) (eval-prim prim-id a*))
               (`(,(? closure-tag?) (lambda ,x ,body) ,env^)
                 (let ((res (match x
                              ((and (not (? symbol?)) params)
                               (extend-env* params a* env^))
                              (sym `((val . (,sym . ,a*)) . ,env^)))))
                   (eval-term body res))))))
         (`(quote ,(? quotable? datum)) datum)
         (`(quasiquote ,qqterm) (eval-qq qqterm env))
         (`(if ,condition ,alt-true ,alt-false)
           (if (eval-term condition env)
             (eval-term alt-true env)
             (eval-term alt-false env)))
         ((? term1? `(lambda ,params ,body))
          `(,closure-tag (lambda ,params ,body) ,env))
         (`(let ,_ ,_) (eval-term (let->lambda operation) env))
         ((? term1? `(letrec ,binding* ,letrec-body))
          (eval-term letrec-body `((rec . ,binding*) . ,env))))))))

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
  (check-equal? (eval-term '(let ((p (cons 8 9))) (cdr p)) initial-env) 9)
  (check-equal?
    (eval-term
      '(letrec ((append
                  (lambda (xs ys)
                    (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
         (list (append '() '()) (append '(foo) '(bar)) (append '(1 2) '(3 4))))
      initial-env)
    '(() (foo bar) (1 2 3 4)))
  (check-equal?  (eval-term '`(1 ,(car `(,(cdr '(b 2)) 3)) ,'a) initial-env)
    '(1 (2) a))
  )

; the goal is to support something like this interpreter

;(let ((closure-tag (gensym "#%closure"))
      ;(prim-tag (gensym "#%primitive"))
      ;(empty-env '())
      ;(initial-env `((cons . (val . (,prim-tag . cons)))
                     ;(car . (val . (,prim-tag . car)))
                     ;(cdr . (val . (,prim-tag . cdr)))
                     ;(null? . (val . (,prim-tag . null?)))
                     ;(pair? . (val . (,prim-tag . pair?)))
                     ;(symbol? . (val . (,prim-tag . symbol?)))
                     ;(not . (val . (,prim-tag . not)))
                     ;(equal? . (val . (,prim-tag . equal?)))
                     ;(list . (val . (,closure-tag (lambda x x) ,empty-env)))
                     ;. ,empty-env)))

;(letrec
  ;((closure-tag? (lambda (v) (equal? v closure-tag)))
   ;(prim-tag? (lambda (v) (equal? v prim-tag)))
   ;(applicable-tag? (lambda (v) (or (closure-tag? v) (prim-tag? v))))
   ;(quotable? (lambda (v)
                ;(match v
                  ;((? symbol?) (not (applicable-tag? v)))
                  ;(`(,a . ,d) (and (quotable? a) (quotable? d)))
                  ;(_ #t))))
   ;(not-in-params? (lambda (ps sym)
                     ;(match ps
                       ;('() #t)
                       ;(`(,a . ,d)
                         ;(and (not (equal? a sym)) (not-in-params? d sym))))))
   ;(param-list? (lambda (x)
                  ;(match x
                    ;('() #t)
                    ;(`(,(? symbol? a) . ,d)
                      ;(and (param-list? d) (not-in-params? d a)))
                    ;(_ #f))))
   ;(params? (lambda (x)
              ;(match-ws x
                ;(10 (? param-list?) #t)
                ;(1 x (symbol? x)))))
   ;(in-env? (lambda (env sym)
              ;(match env
                ;('() #f)
                ;(`((,a . ,_). ,d) (or (equal? a sym) (in-env? d sym))))))
   ;(extend-env* (lambda (params args env)
                  ;(match `(,params . ,args)
                    ;(`(() . ()) env)
                    ;(`((,x . ,dx*) . (,a . ,da*))
                      ;(extend-env* dx* da* `((,x . (val . ,a)) . ,env))))))
   ;(lookup (lambda (env sym)
             ;(match env
               ;(`((,y . ,b) . ,rest)
                 ;(if (equal? sym y)
                   ;(match b
                     ;(`(val . ,v) v)
                     ;(`(rec . ,lam-expr) `(,closure-tag ,lam-expr ,env)))
                   ;(lookup rest sym))))))
   ;(term?
     ;(lambda (term env)
       ;(letrec ((term1? (lambda (v) (term? v env)))
                ;(terms? (lambda (ts env)
                       ;(match ts
                         ;('() #t)
                         ;(`(,t . ,ts) (and (term? t env) (terms? ts env)))))))
         ;(match term
           ;(#t #t)
           ;(#f #t)
           ;((? number?) #t)
           ;((and (? symbol? sym)) (in-env? env sym))
           ;(`(,(? term1?) . ,rands) (terms? rands env))
           ;(`(quote ,datum) (quotable? datum))
           ;(`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
           ;(`(lambda ,params ,body)
             ;(and (params? params)
                  ;(let ((res (match params
                               ;((and (not (? symbol?)) params)
                                ;(extend-env* params params env))
                               ;(sym `((,sym . (val . ,sym)) . ,env)))))
                    ;(term? body res))))
           ;(`(letrec ((,p-name ,(and `(lambda ,params ,body) lam-expr)))
               ;,letrec-body)
             ;(and (params? params)
                  ;(let ((res `((,p-name . (rec . (lambda ,params ,body)))
                               ;. ,env)))
                    ;(and (term? lam-expr res) (term? letrec-body res)))))
           ;(_ #f)))))
   ;(eval-prim
     ;(lambda (prim-id args)
       ;(match `(,prim-id . ,args)
         ;(`(cons ,a ,d) `(,a . ,d))
         ;(`(car (,(and (not (? applicable-tag?)) a) . ,d)) a)
         ;(`(cdr (,(and (not (? applicable-tag?)) a) . ,d)) d)
         ;(`(null? ,v) (match v ('() #t) (_ #f)))
         ;(`(pair? ,v) (match v (`(,(not (? applicable-tag?)) . ,_) #t) (_ #f)))
         ;(`(symbol? ,v) (symbol? v))
         ;(`(number? ,v) (number? v))
         ;(`(not ,v) (match v (#f #t) (_ #f)))
         ;(`(equal? ,v1 ,v2) (equal? v1 v2)))))
   ;(eval-term-list
     ;(lambda (terms env)
       ;(match terms
         ;('() '())
         ;(`(,term . ,terms)
           ;`(,(eval-term term env) . ,(eval-term-list terms env))))))
   ;(eval-term
     ;(lambda (term env)
       ;(let ((bound? (lambda (sym) (in-env? env sym)))
             ;(term1? (lambda (v) (term? v env))))
         ;(tagged `(eval-term ,term ,env)
           ;(match-cdfs term
             ;(1 #t #t)
             ;(1 #f #f)
             ;(1 (? number? num) num)
             ;(1 `(,(and 'quote (not (? bound?))) ,(? quotable? datum)) datum)
             ;(1 (? symbol? sym) (lookup env sym))
             ;(1 (and `(,op . ,_) operation)
               ;(match-cws operation
                 ;(0 10 `(,(or (? bound?) (not (? symbol?))) . ,rands)
                  ;(let ((op (eval-term op env))
                        ;(a* (eval-term-list rands env)))
                    ;(match-c op
                      ;(0 `(,(? prim-tag?) . ,prim-id) (eval-prim prim-id a*))
                      ;(0 `(,(? closure-tag?) (lambda ,x ,body) ,env^)
                       ;(let ((res (match-cws x
                                    ;(0 10 (not (? symbol? params))
                                     ;(extend-env* params a* env^))
                                    ;(0 1 sym `((,sym . (val . ,a*)) . ,env^)))))
                         ;(eval-term body res))))))
                 ;(1 10 `(if ,condition ,alt-true ,alt-false)
                  ;(if (eval-term condition env)
                    ;(eval-term alt-true env)
                    ;(eval-term alt-false env)))
                 ;(1 1 (? term1? `(lambda ,params ,body))
                  ;`(,closure-tag (lambda ,params ,body) ,env))
                 ;(1 1 (? term1? `(letrec ((,p-name (lambda ,params ,body)))
                                   ;,letrec-body))
                  ;(eval-term
                    ;letrec-body `((,p-name . (rec . (lambda ,params ,body)))
                                  ;. ,env)))))))))))

  ;; TODO: main entry into eval-term ...
  ;; start with proper-env check

  ;)
;)
