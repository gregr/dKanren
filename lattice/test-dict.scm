(load "test.scm")
(load "dict.scm")

(define d1 (dict '((a . 1) (5 . 10) (c . 2) (b . 3))))
(define d2 (dict '((a . 2) (5 . 10) (d . ok) (c . 4))))

(define (add-values k x y) (+ x y))

(test 'dict-1
  d1
  '((a . 1) (b . 3) (c . 2) (5 . 10)))
(test 'dict-2
  d2
  '((a . 2) (c . 4) (d . ok) (5 . 10)))

(test 'dict-join-1
  (dict-join id-value id-value #f dict-empty d1)
  d1)
(test 'dict-join-2
  (dict-join id-value id-value #f d1 dict-empty)
  d1)
(test 'dict-join-3
  (dict-join id-value id-value add-values d1 d2)
  '((a . 3) (b . 3) (c . 6) (d . ok) (5 . 20)))

(test 'dict-meet-1
  (dict-meet add-values dict-empty d1)
  dict-empty)
(test 'dict-meet-2
  (dict-meet add-values d1 dict-empty)
  dict-empty)
(test 'dict-meet-3
  (dict-meet add-values d1 d2)
  '((a . 3) (c . 6) (5 . 20)))

(test 'dict-subtract-1
  (dict-subtract d1 '(c 5))
  '((a . 1) (b . 3)))

(test 'dict-project-1
  (dict-project d1 '(c 5))
  '((c . 2) (5 . 10)))
