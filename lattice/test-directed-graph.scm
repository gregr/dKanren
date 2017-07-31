(load "test.scm")
(load "directed-graph.scm")

(define dg1 (dg-add dg-empty 'a 'b))
(define dg2 (dg-add dg1 'a 'c))
(define dg3 (dg-add dg2 'd 'c))
(define dg4 (dg-add dg3 'b 'd))
(define dg5 (dg-add dg4 'e 'b))

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
(test 'add-6
  (dg-add-simplify dg4 'b 'c)
  '((a . (b c)) (b . (d)) (d . (c))))
(test 'add-7
  (dg-add-simplify dg2 'b 'c)
  '((a . (b)) (b . (c))))
(test 'add-8
  (dg-add-simplify dg3 'b 'd)
  '((a . (b)) (b . (d)) (d . (c))))

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

(test 'replace-1
  (dg-replace dg4 '(a b) 'g)
  '((d . (c)) (g . (c d))))
(test 'replace-2
  (dg-replace dg5 '(a b) 'g)
  '((d . (c)) (e . (g)) (g . (c d))))

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

(test 'scc-1
  (dg-scc dg-empty 'c 'b)
  '())
(test 'scc-2
  (dg-scc dg1 'c 'b)
  '())
(test 'scc-3
  (dg-scc dg2 'c 'b)
  '())
(test 'scc-4
  (dg-scc dg3 'c 'b)
  '())
(test 'scc-5
  (dg-scc dg4 'c 'b)
  '(b c d))
(test 'scc-5
  (dg-scc dg4 'c 'a)
  '(a b c d))

(define n1 (dg-transitive-add dg-empty 'a 'b))
(define tdg1 (cdr n1))
(define n2 (dg-transitive-add tdg1 'a 'c))
(define tdg2 (cdr n2))
(define n3 (dg-transitive-add tdg2 'd 'c))
(define tdg3 (cdr n3))
(define n4 (dg-transitive-add tdg3 'b 'd))
(define tdg4 (cdr n4))
(define n5 (dg-transitive-add tdg4 'e 'b))
(define tdg5 (cdr n5))
(define n6 (dg-transitive-add tdg5 'f 'e))
(define tdg6 (cdr n6))

(test 'transitive-add-1
  n1
  '(#f . ((a . (b)))))
(test 'transitive-add-2
  n2
  '(#f . ((a . (b c)))))
(test 'transitive-add-3
  n3
  '(#f . ((a . (b c)) (d . (c)))))
(test 'transitive-add-4
  n4
  '(#f . ((a . (b)) (b . (d)) (d . (c)))))
(test 'transitive-add-5
  n5
  '(#f . ((a . (b)) (b . (d)) (d . (c)) (e . (b)))))
(test 'transitive-add-6
  n6
  '(#f . ((a . (b)) (b . (d)) (d . (c)) (e . (b)) (f . (e)))))
(test 'transitive-add-7
  (dg-transitive-add tdg6 'b 'e)
  '((b e) . ((a . (b)) (b . (d)) (d . (c)) (f . (b)))))
(test 'transitive-add-8
  (dg-transitive-add tdg6 'd 'a)
  '((a b d) . ((a . (c)) (e . (a)) (f . (e)))))
