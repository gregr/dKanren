#lang racket/base
(provide

  )

(require
  racket/match
  )

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
(define prim-tag (gensym "#%primitive"))

(define (applicable-tag? v)
  (or (equal? closure-tag v) (equal? prim-tag v)))
(define (quotable? v)
  (match v
    ((? symbol?) (not (applicable-tag? v)))
    (`(,a . ,d) (and (quotable? a) (quotable? d)))
    (_ #t)))

(define empty-env '())
(define initial-env `((cons . (val . (,prim-tag . cons)))
                      (car . (val . (,prim-tag . car)))
                      (cdr . (val . (,prim-tag . cdr)))
                      (null? . (val . (,prim-tag . null?)))
                      (pair? . (val . (,prim-tag . pair?)))
                      (symbol? . (val . (,prim-tag . symbol?)))
                      (not . (val . (,prim-tag . not)))
                      (equal? . (val . (,prim-tag . equal?)))
                      (list . (val . (,closure-tag (lambda x x) ,empty-env)))
                      . ,empty-env))
(define (in-env? env sym)
  (match env
    ('() #f)
    (`((,a . ,_) . ,d) (or (equal? a sym) (in-env? d sym)))))
(define (extend-env* params args env)
  (match `(,params . ,args)
    (`(() . ()) env)
    (`((,x . ,dx*) . (,a . ,da*))
      (extend-env* dx* da* `((,x . (val . ,a)) . ,env)))))
(define (lookup env sym)
  (match env
    (`((,y . ,b) . ,rest)
      (if (equal? sym y)
        (match b
          (`(val . ,v) v)
          (`(rec . ,lam-expr) `(,closure-tag ,lam-expr ,env)))
        (lookup rest sym)))))

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

(define (term? term env)
  (letrec ((term1? (lambda (v) (term? v env)))
           (terms? (lambda (ts env)
                     (match ts
                       ('() #t)
                       (`(,t . ,ts) (and (term? t env) (terms? ts env)))))))
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
             (let ((res (match params
                          ((not (? symbol? params))
                           (extend-env* params params env))
                          (sym `((,sym . (val . ,sym)) . ,env)))))
               (term? body res))))
      (`(letrec ((,p-name ,(and `(lambda ,params ,body) lam-expr)))
          ,letrec-body)
        (and (params? params)
             (let ((res `((,p-name . (rec . (lambda ,params ,body)))
                          . ,env)))
               (and (term? lam-expr res) (term? letrec-body res)))))
      (_ #f))))

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
