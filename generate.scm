(load "transparent-evalo.scm")

(define atoms
  '(() #t #f s quote app var lambda list cons car cdr closure 1 x y))

(define-relation (atom-from xs x)
  (fresh (next rest)
    (== `(,next . ,rest) xs)
    (conde
      ((== next x))
      ((atom-from rest x)))))

(define-relation (term x)
  (conde
    ((atom-from atoms x))
    ((fresh (a d)
       (== `(,a . ,d) x)
       (term a)
       (term d)))))

(define-relation (list-of domain xs)
  (conde
    ((== '() xs))
    ((fresh (a d)
       (== `(,a . ,d) xs)
       (domain a)
       (list-of domain d)))))

(define (term-list xs) (list-of term xs))
(define (vref x) (list-of (lambda (x) (== 's x)) x))

(define-relation (example-lookupo x)
  (fresh (index env value)
    (== `(lookupo ,index ,env ,value) x)
    (vref index)
    (term-list env)
    (term value)))

(define-relation (example-eval-expo x)
  (fresh (expr env value)
    (== `(eval-expo ,expr ,env ,value) x)
    (term expr)
    (term-list env)
    (term value)))

(define-relation (example-eval-listo x)
  (fresh (e* env value)
    (== `(eval-listo ,e* ,env ,value) x)
    (term-list e*)
    (term-list env)
    (term value)))

(define-relation (example-== x)
  (fresh (a d)
    (== `(== ,a ,d) x)
    (term a)
    (term d)))

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
              (let ((input (car (reify-initial (car next)))))
                (k `(,(if (null? (run 1 (q) (test input))) 0 1) ,input)))
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
