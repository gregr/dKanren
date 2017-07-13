(load "test.scm")
(load "discrete-set.scm")

(define items '(b a #t #f 3 2 "ok" () "ab" (#f 3) (#f 1) ((#t) . 5)))
(define items-ordered '(((#t) . 5) (#f 1) (#f 3) a b "ab" "ok" 2 3 #f #t ()))
(define with-items (discrete-set-with items))
(define without-items (discrete-set-without items))

(test 'discrete-set-1
  with-items
  `(#f . ,items-ordered))
(test 'discrete-set-2
  without-items
  `(#t . ,items-ordered))
(test 'discrete-set-3
  (discrete-set-complement with-items)
  `(#t . ,items-ordered))
(test 'discrete-set-4
  (discrete-set-complement without-items)
  `(#f . ,items-ordered))

(define items2 '(c a #t 5 2 "ok" "abc" (#f 3) (#f 2) ((#t) . 5)))
(define with-items2 (discrete-set-with items2))

(define items-all '(b a c #t 5 #f 3 2 "ok" "abc" () "ab" (#f 2) (#f 3) (#f 1) ((#t) . 5)))
(define with-items-all (discrete-set-with items-all))

(define items-missing '(b #f 3 "ab" () (#f 1)))
(define with-items-missing (discrete-set-with items-missing))
(define without-items-missing (discrete-set-complement with-items-missing))

(test 'discrete-set-join-1
  (discrete-set-join discrete-set-full discrete-set-full)
  discrete-set-full)
(test 'discrete-set-join-2
  (discrete-set-join discrete-set-full discrete-set-empty)
  discrete-set-full)
(test 'discrete-set-join-3
  (discrete-set-join discrete-set-empty discrete-set-full)
  discrete-set-full)
(test 'discrete-set-join-4
  (discrete-set-join discrete-set-empty discrete-set-empty)
  discrete-set-empty)
(test 'discrete-set-join-5
  (discrete-set-join with-items discrete-set-empty)
  with-items)
(test 'discrete-set-join-6
  (discrete-set-join discrete-set-empty with-items)
  with-items)
(test 'discrete-set-join-7
  (discrete-set-join with-items discrete-set-full)
  discrete-set-full)
(test 'discrete-set-join-8
  (discrete-set-join discrete-set-full with-items)
  discrete-set-full)
(test 'discrete-set-join-9
  (discrete-set-join with-items with-items)
  with-items)
(test 'discrete-set-join-10
  (discrete-set-join with-items without-items)
  discrete-set-full)
(test 'discrete-set-join-11
  (discrete-set-join without-items with-items)
  discrete-set-full)
(test 'discrete-set-join-12
  (discrete-set-join with-items with-items2)
  with-items-all)
(test 'discrete-set-join-13
  (discrete-set-join with-items2 with-items)
  with-items-all)
(test 'discrete-set-join-14
  (discrete-set-join without-items with-items2)
  without-items-missing)
(test 'discrete-set-join-15
  (discrete-set-join with-items2 without-items)
  without-items-missing)

(define items-overlapping '(a #t 2 "ok" (#f 3) ((#t) . 5)))
(define with-items-overlapping (discrete-set-with items-overlapping))

(define items2-only '(c 5 "abc" (#f 2)))
(define with-items2-only (discrete-set-with items2-only))

(test 'discrete-set-meet-1
  (discrete-set-meet discrete-set-full discrete-set-full)
  discrete-set-full)
(test 'discrete-set-meet-2
  (discrete-set-meet discrete-set-full discrete-set-empty)
  discrete-set-empty)
(test 'discrete-set-meet-3
  (discrete-set-meet discrete-set-empty discrete-set-full)
  discrete-set-empty)
(test 'discrete-set-meet-4
  (discrete-set-meet discrete-set-empty discrete-set-empty)
  discrete-set-empty)
(test 'discrete-set-meet-5
  (discrete-set-meet with-items discrete-set-empty)
  discrete-set-empty)
(test 'discrete-set-meet-6
  (discrete-set-meet discrete-set-empty with-items)
  discrete-set-empty)
(test 'discrete-set-meet-7
  (discrete-set-meet with-items discrete-set-full)
  with-items)
(test 'discrete-set-meet-8
  (discrete-set-meet discrete-set-full with-items)
  with-items)
(test 'discrete-set-meet-9
  (discrete-set-meet with-items with-items)
  with-items)
(test 'discrete-set-meet-10
  (discrete-set-meet with-items without-items)
  discrete-set-empty)
(test 'discrete-set-meet-11
  (discrete-set-meet without-items with-items)
  discrete-set-empty)
(test 'discrete-set-meet-12
  (discrete-set-meet with-items with-items2)
  with-items-overlapping)
(test 'discrete-set-meet-13
  (discrete-set-meet with-items2 with-items)
  with-items-overlapping)
(test 'discrete-set-meet-14
  (discrete-set-meet without-items with-items2)
  with-items2-only)
(test 'discrete-set-meet-15
  (discrete-set-meet with-items2 without-items)
  with-items2-only)
