(load "test.scm")
(load "directed-graph.scm")

(define dg1 (dg-add dg-empty 'a 'b))
(define dg2 (dg-add dg1 'a 'c))
(define dg3 (dg-add dg2 'd 'c))
(define dg4 (dg-add dg3 'b 'd))

(test 'add-1
  dg1
  '((a . (b))))
(test 'add-2
  (dg-add dg1 'a 'b)
  '((a . (b))))
(test 'add-3
  dg2
  '((a . (b c))))
(test 'add-4
  dg3
  '((a . (b c)) (d . (c))))
(test 'add-5
  dg4
  '((a . (b c)) (b . (d)) (d . (c))))

(test 'remove-1
  (dg-remove dg-empty 'x 'y)
  dg-empty)
(test 'remove-2
  (dg-remove dg1 'a 'b)
  dg-empty)
(test 'remove-3
  (dg-remove dg1 'a 'c)
  '((a . (b))))
(test 'remove-3
  (dg-remove dg2 'a 'b)
  '((a . (c))))

(test 'succ-1
  (dg-succ dg-empty 'a)
  '())
(test 'succ-2
  (dg-succ dg1 'a)
  '(b))
(test 'succ-3
  (dg-succ dg3 'a)
  '(b c))
(test 'succ-4
  (dg-succ dg4 'b)
  '(d))
(test 'succ-5
  (dg-succ* dg4 'b)
  '(c d))

(test 'pred-1
  (dg-pred dg-empty 'a)
  '())
(test 'pred-2
  (dg-pred dg1 'a)
  '())
(test 'pred-3
  (dg-pred dg2 'c)
  '(a))
(test 'pred-4
  (dg-pred dg4 'c)
  '(a d))
(test 'pred-5
  (dg-pred* dg4 'c)
  '(a b d))
