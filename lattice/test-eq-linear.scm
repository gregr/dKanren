(load "test.scm")
(load "eq-linear.scm")

;; v + w = 10
(define e1 (eq-sparse->dense '((0 . 1) (1 . 1) . 10)))
;; x + y + z = 4
(define e2 (eq-sparse->dense '((2 . 1) (3 . 1) (4 . 1) . 4)))
;; w + z = 8
(define e3 (eq-sparse->dense '((1 . 1) (4 . 1) . 8)))
;; 3x + 3y = 6
(define e4 (eq-sparse->dense '((2 . 3) (3 . 3) . 6)))

(define n1 (eqs-linear-add eqs-empty e1))
(define eqs1 (eqs-next-eqs n1))
(define n2 (eqs-linear-add eqs1 e2))
(define eqs2 (eqs-next-eqs n2))
(define n3 (eqs-linear-add eqs2 e3))
(define eqs3 (eqs-next-eqs n3))
(define n4 (eqs-linear-add eqs3 e4))
(define eqs4 (eqs-next-eqs n4))

(test 'solved-1
  (eqs-next-solved n1)
  '())
(test 'solved-2
  (eqs-next-solved n2)
  '())
(test 'solved-3
  (eqs-next-solved n3)
  '())
(test 'solved-4
  (eqs-next-solved n4)
  '((0 . 4) (1 . 6) (4 . 2)))  ;; v = 4, w = 6, z = 2

(test 'next-eqs-1
  (length eqs1)
  1)
(test 'next-eqs-2
  (length eqs2)
  2)
(test 'next-eqs-3
  (length eqs3)
  3)
(test 'next-eqs-4
  (length eqs4)
  1)  ;; x and y are still-unknown

(test 'next-eqs-size-1
  (map eq-size eqs1)
  '(2))
(test 'next-eqs-size-2
  (map eq-size eqs2)
  '(5 5))
(test 'next-eqs-size-3
  (map eq-size eqs3)
  '(5 5 5))
(test 'next-eqs-size-4
  (map eq-size eqs4)
  '(2))  ;; x and y are still-unknown
