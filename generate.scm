(load "transparent-evalo.scm")

;; Max term size is roughly 2^max-term-depth.
(define max-term-depth 3)  ;; Set this to #f for unlimited depth.

(define lvars
  (list (var -100) (var -101) (var -102)))

(define atoms
  '(() #t #f s quote app var lambda list cons car cdr closure 1 x y))

(define-relation (element-from xs x)
  (fresh (next rest)
    (== `(,next . ,rest) xs)
    (conde
      ((== next x))
      ((element-from rest x)))))

(define-relation (term n x)
  (if (and n (< 0 n))
    (conde
      ;; Reorder clauses to control frequency of occurrence.
      ((element-from atoms x))
      ((element-from lvars x))  ;; Comment this out to disable vars.
      ((fresh (a d)
         (== `(,a . ,d) x)
         (term (- n 1) a)
         (term (- n 1) d)))
      )
    fail))

(define-relation (list-of domain xs)
  (conde
    ((== '() xs))
    ((fresh (a d)
       (== `(,a . ,d) xs)
       (domain a)
       (list-of domain d)))))

(define (term-list n xs) (list-of (lambda (x) (term n x)) xs))
(define (vref x) (list-of (lambda (x) (== 's x)) x))

(define-relation (example-lookupo x)
  (fresh (index env value)
    (== `(lookupo ,index ,env ,value) x)
    (vref index)
    (term-list max-term-depth env)
    (term max-term-depth value)))

(define-relation (example-eval-expo x)
  (fresh (expr env value)
    (== `(eval-expo ,expr ,env ,value) x)
    (term max-term-depth expr)
    (term-list max-term-depth env)
    (term max-term-depth value)))

(define-relation (example-eval-listo x)
  (fresh (e* env value)
    (== `(eval-listo ,e* ,env ,value) x)
    (term-list max-term-depth e*)
    (term-list max-term-depth env)
    (term max-term-depth value)))

(define-relation (example-== x)
  (fresh (a d)
    (== `(== ,a ,d) x)
    (term max-term-depth a)
    (term max-term-depth d)))

(define (examples count generate)
  (define (test example)
    (eval (cons (car example) (map (lambda (x) `(quote ,x)) (cdr example)))))
  (define inputs (map car (run count (x) (generate x))))
  (map
    (lambda (i) `(,(if (null? (run 1 (q) (test i))) 0 1) ,i))
    inputs))


;; Lazy example streaming

(define examples-current '())

(define (examples-start generate)
  (set! examples-current (query (i) (generate i))))

(define (examples-next k)
  (define (test example)
    (eval (cons (car example) (map (lambda (x) `(quote ,x)) (cdr example)))))
  (and (not (null? examples-current))
       (let ((next (stream-next examples-current)))
         (and (pair? next)
              (set! examples-current (cdr next))
              (let ((input (car (walk* (car next) var-initial))))
                (k `(,(if (null? (run 1 (q) (test input))) 0 1)
                      ,(reify 0 state-empty input))))
              #t))))


;; Stream unification examples

(examples-start example-==)

;; Optionally set n to the number of desired examples.
(let loop ((n #f))
  (if (and n (= 0 n))
    #f
    (begin
      (and (examples-next (lambda (x) (printf "~s\n" x)))
           (loop (and n (- n 1)))))))
