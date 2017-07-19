(load "transparent-evalo.scm")

;; Max term size is roughly 2^max-term-depth.
(define max-term-depth 3)

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

(define-relation (term m n x)
  (if (<= m n)
    (conde
      ;; Reorder clauses to control frequency of occurrence.
      ((== 1 n) (element-from atoms x))
      ((== 1 n) (element-from lvars x))  ;; Comment this out to disable vars.
      ((fresh (a d)
         (== `(,a . ,d) x)
         (term 1 (- n 1) a)
         (term 1 (- n 1) d)))
      ((fresh (a d)
         (== `(,a . ,d) x)
         (term 1 (- n 1) d)
         (term 2 (- n 1) a)))
      ((term m (- n 1) x))  ;; Remove this clause to enable extreme bushiness.
      )
    fail))

(define-relation (list-of domain xs)
  (conde
    ((== '() xs))
    ((fresh (a d)
       (== `(,a . ,d) xs)
       (domain a)
       (list-of domain d)))))

(define (term-list n xs) (list-of (lambda (x) (term 1 n x)) xs))
(define (vref x) (list-of (lambda (x) (== 's x)) x))

(define-relation (example-lookupo x)
  (fresh (index env value)
    (== `(lookupo ,index ,env ,value) x)
    (vref index)
    (term-list max-term-depth env)
    (term 1 max-term-depth value)))

(define-relation (example-eval-expo x)
  (fresh (expr env value)
    (== `(eval-expo ,expr ,env ,value) x)
    (term 1 max-term-depth expr)
    (term-list max-term-depth env)
    (term 1 max-term-depth value)))

(define-relation (example-eval-listo x)
  (fresh (e* env value)
    (== `(eval-listo ,e* ,env ,value) x)
    (term-list max-term-depth e*)
    (term-list max-term-depth env)
    (term 1 max-term-depth value)))

(define-relation (example-== x)
  (conde
    ((fresh (a d)
       (== `(== ,a ,d) x)
       (term 1 max-term-depth a)
       (term 1 max-term-depth d)))
    ((fresh (a d)
       (== `(== ,a ,d) x)
       (term 1 max-term-depth d)
       (term 2 max-term-depth a)))))

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
                (k `(,(if (null? (run 1 (q) (test input))) 0 1) ,input)))
              #t))))


;; Stream unification examples

(examples-start example-==)

(define (print-reified x) (printf "~s\n" (reify 0 state-empty x)))
(define (print-==-as-branch x)
  (define branch-var (var -200))
  (when (= 0 (car x))
    (let ((a (cadr (cadr x)))
          (c (caddr (cadr x))))
      (print-reified
        `(conj (== ,branch-var ,a)
               (disj (== ,branch-var ,a) (== ,branch-var ,c)))))))

;; Optionally set n to the number of desired examples.
(let loop ((n #f))
  (if (and n (= 0 n))
    #f
    (begin
      (and (examples-next print-==-as-branch)
           (loop (and n (- n 1)))))))
