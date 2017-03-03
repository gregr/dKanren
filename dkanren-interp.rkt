#lang racket/base
(provide
  evalo
  )

(require
  "dkanren.rkt"
  )

; TODO
; profile to see what terms are getting all the attention
; nest all literals under a single match clause to compress their mostly-useless scheduling
; can a weighted match prioritize symbol lookup unraveling?
; additional predicates: procedure?, boolean?

; evalo solver
;   tag match statements
;   recognize tag groups (confirm via debug printing)
;   design partitions and env analysis
;     force fresh variable names as needed
;   rules
;     unshadowed env
;     literals
;     car/cdr reachables
;     pair? before car/cdr when input type is ambiguous in env/partition
;       rather, car/cdr only allowed on unambiguous pairs
;       conditionals on other type/equality witnesses may create partitions with reduced ambiguity
;     never car/cdr a cons result
;     never type-witness a literal
;     never equate two literals
;     what about generating lambdas/letrecs?
;       (aggressive/permissive) union type system?

; related work
;   escher
;   myth
;   http://leon.epfl.ch/doc/
;   https://emina.github.io/rosette/
;   https://people.eecs.berkeley.edu/~bodik/research/pldi07-sketching-stencils.pdf
;   http://acypher.com/wwid/Chapters/07Metamouse.html
;   http://web.media.mit.edu/~lieber/Lieberary/Mondrian/Mondrian.html
;   Chimera?

(define (letrec-eval-term program)
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
                       (match/lazy v
                         ((? symbol?) (not (applicable-tag? v)))
                         (`(,a . ,d) (and (quotable? a) (quotable? d)))
                         (_ #t))))
          (not-in-params? (lambda (ps sym)
                            (match/lazy ps
                              ('() #t)
                              (`(,a . ,d)
                                (and (not (equal? a sym))
                                     (not-in-params? d sym))))))
          (param-list? (lambda (x)
                         (match/lazy x
                           ('() #t)
                           (`(,(? symbol? a) . ,d)
                             (and (param-list? d) (not-in-params? d a)))
                           (_ #f))))
          (params? (lambda (x)
                     (match/lazy x
                       ((? param-list?) #t)
                       (x (symbol? x)))))
          (in-env? (lambda (env sym)
                     (match/lazy env
                       ('() #f)
                       (`((,a . ,_) . ,d)
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
                           (match/lazy ts
                             ('() #t)
                             (`(,t . ,ts)
                               (and (term? t env) (terms? ts env)))))))
                (match/lazy term
                  (#t #t)
                  (#f #t)
                  ((number) #t)
                  ((symbol sym) (in-env? env sym))
                  (`(,(? term1?) . ,rands) (terms? rands env))
                  (`(quote ,datum) (quotable? datum))
                  (`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
                  (`(lambda ,params ,body)
                    (and (params? params)
                         (let ((res
                                 (match params
                                   ((and (not (symbol)) params)
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
                (`(null? ()) #t)
                (`(null? ,_) #f)
                (`(pair? (,(not (? applicable-tag?)) . ,_)) #t)
                (`(pair? ,_) #f)
                (`(symbol? ,(symbol)) #t)
                (`(symbol? ,_) #f)
                (`(number? ,(number)) #t)
                (`(number? ,(number)) #f)
                (`(not #f) #t)
                (`(not #t) #f)
                (`(equal? ,v1 ,v1) #t)
                (`(equal? ,_ ,_) #f))))
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
                  ((symbol sym) (lookup env sym))
                  (#t #t)
                  (#f #f)
                  ((number num) num)
                  (`(,(and 'quote (not (? bound?))) ,(? quotable? datum))
                    datum)
                  ((and `(,op . ,_) operation)
                   (match operation
                     (`(,(or (not (symbol)) (? bound?))
                         . ,rands)
                       (let ((op (eval-term op env))
                             (a* (eval-term-list rands env)))
                         (match op
                           (`(,(? prim-tag?) . ,prim-id)
                             (eval-prim prim-id a*))
                           (`(,(? closure-tag?) (lambda ,x ,body) ,env^)
                             (let ((res (match x
                                          ((and (not (symbol)) params)
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

         (let ((program ',program))
           (let ((_ (match/lazy (term? program initial-env) (#t #t))))
             (eval-term program initial-env)))))))

(define (evalo program result)
  (let ((tm (letrec-eval-term program)))
    (dk-evalo tm result)))

(module+ test
  (require
    racket/pretty
    rackunit
    )

  (define-syntax test
    (syntax-rules ()
     ((_ name expr expected)
      (let ((actual expr))
        (when (not (equal? actual expected))
          (display name)
          (newline)
          (pretty-print actual)
          (newline))
        (check-equal? actual expected)))))

  (define (letrec-append body)
    `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       ,body))

  (test "evalo-1"
    (run* (q)
      (evalo `'(1 2 ,q 4 5) '(1 2 3 4 5)))
    '((3)))
  (test "evalo-append-0"
    (run* (q)
      (evalo (letrec-append
               '(list (append '() '())
                      (append '(foo) '(bar))
                      (append '(1 2) '(3 4))))
             q))
    '(((() (foo bar) (1 2 3 4)))))
  (test "evalo-append-1"
    (run* (q)
      (evalo (letrec-append `(append '(1 2 3) '(4 5))) q))
    '(((1 2 3 4 5))))
  (test "evalo-append-2"
    (run* (q)
      (evalo (letrec-append `(append '(1 2 3) ',q)) '(1 2 3 4 5)))
    '(((4 5))))
  (test "evalo-append-3"
    (run* (q)
      (evalo (letrec-append `(append ',q '(4 5))) '(1 2 3 4 5)))
    '(((1 2 3))))
  (test "evalo-append-4"
    (run* (q r)
      (evalo (letrec-append `(append ',q ',r)) '(1 2 3 4 5)))
    '((() (1 2 3 4 5))
      ((1) (2 3 4 5))
      ((1 2) (3 4 5))
      ((1 2 3) (4 5))
      ((1 2 3 4) (5))
      ((1 2 3 4 5) ())))

  (test "evalo-append-synthesis-1"
    (run 1 (q)
      (evalo `(letrec
                ((append (lambda (xs ys)
                           (if (null? xs)
                             ys
                             (cons (car ,q) (append (cdr xs) ys))))))
                (append '(1 2) '(3 4)))
             '(1 2 3 4))
      )
    '((xs)))
  (test "evalo-append-synthesis-2"
    (run 1 (q)
      (evalo `(letrec
                ((append (lambda (xs ys)
                           (if (null? xs)
                             ys
                             (cons (car xs) (,q (cdr xs) ys))))))
                (append '(1 2) '(3 4)))
             '(1 2 3 4))
      )
    '((append)))
  (test "evalo-append-synthesis-3"
    (run 1 (q)
      (evalo `(letrec
                ((append (lambda (xs ys)
                           (if (,q xs)
                             ys
                             (cons (car xs) (append (cdr xs) ys))))))
                (append '(1 2) '(3 4)))
             '(1 2 3 4))
      )
    '((null?)))

  ;; TODO: run higher order interpreters in the relational interpreter instead.
  ;; This won't work directly due to dKanren's first-order restriction.
  ;(define ex-eval-expr
    ;'(letrec
        ;((eval-expr
          ;(lambda (expr env)
            ;(match expr
              ;(`(quote ,datum) datum)
              ;(`(lambda (,(? symbol? x)) ,body)
                ;(lambda (a)
                  ;(eval-expr body (lambda (y)
                                    ;(if (equal? y x) a (env y))))))
              ;((? symbol? x) (env x))
              ;(`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env)))
              ;(`(,rator ,rand) ((eval-expr rator env)
                                ;(eval-expr rand env)))))))
        ;(list
          ;(eval-expr '((lambda (y) y) 'g1) 'initial-env)
          ;(eval-expr '(((lambda (z) z) (lambda (v) v)) 'g2) 'initial-env)
          ;(eval-expr '(((lambda (a) (a a)) (lambda (b) b)) 'g3) 'initial-env)
          ;(eval-expr '(((lambda (c) (lambda (d) c)) 'g4) 'g5) 'initial-env)
          ;(eval-expr '(((lambda (f) (lambda (v1) (f (f v1)))) (lambda (e) e)) 'g6) 'initial-env)
          ;(eval-expr '((lambda (g) ((g g) g)) (lambda (i) (lambda (j) 'g7))) 'initial-env))))
  ;(test-eval ex-eval-expr '(g1 g2 g3 g4 g6 g7))

  ;(define ex-eval-expr-dneg
    ;'(letrec
        ;((eval-expr
          ;(lambda (expr env)
            ;(match expr
              ;(`(,(not (not 'quote)) ,datum) datum)
              ;(`(lambda (,(? symbol? x)) ,body)
                ;(lambda (a)
                  ;(eval-expr body (lambda (y)
                                    ;(if (equal? y x) a (env y))))))
              ;((symbol x) (env x))
              ;(`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env)))
              ;(`(,rator ,rand) ((eval-expr rator env)
                                ;(eval-expr rand env)))))))
        ;(list
          ;(eval-expr '((lambda (y) y) 'g1) 'initial-env)
          ;(eval-expr '(((lambda (z) z) (lambda (v) v)) 'g2) 'initial-env)
          ;(eval-expr '(((lambda (a) (a a)) (lambda (b) b)) 'g3) 'initial-env)
          ;(eval-expr '(((lambda (c) (lambda (d) c)) 'g4) 'g5) 'initial-env)
          ;(eval-expr '(((lambda (f) (lambda (v1) (f (f v1)))) (lambda (e) e)) 'g6) 'initial-env)
          ;(eval-expr '((lambda (g) ((g g) g)) (lambda (i) (lambda (j) 'g7))) 'initial-env))))
  ;(test-eval ex-eval-expr-dneg '(g1 g2 g3 g4 g6 g7))
  )
