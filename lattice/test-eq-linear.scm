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

(define eqs1 (eqs-linear-add eqs-empty e1))
(define eqs2 (eqs-linear-add eqs1 e2))
(define eqs3 (eqs-linear-add eqs2 e3))
(define eqs4 (eqs-linear-add eqs3 e4))

(test 'solved-1
  (eqs-linear-solved eqs1)
  '())
(test 'solved-2
  (eqs-linear-solved eqs2)
  '())
(test 'solved-3
  (eqs-linear-solved eqs3)
  '())
(test 'solved-4
  (eqs-linear-solved eqs4)
  '((0 . 4) (1 . 6) (4 . 2)))  ;; v = 4, w = 6, z = 2

(test 'solved-remove-1
  (length (eqs-linear-solved-remove eqs1))
  1)
(test 'solved-remove-2
  (length (eqs-linear-solved-remove eqs2))
  2)
(test 'solved-remove-3
  (length (eqs-linear-solved-remove eqs3))
  3)
(test 'solved-remove-4
  (length (eqs-linear-solved-remove eqs4))
  1)  ;; x + y = still-unknown
