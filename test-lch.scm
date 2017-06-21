(load "lch.scm")

(define-syntax test
  (syntax-rules ()
    ((_ name expr expected-expr)
     (begin
       (printf "Testing ~s: " name)
       (let* ((expected expected-expr) (actual expr))
         (if (equal? expected actual)
           (printf "Succeeded.\n")
           (printf "\nFailed: ~a\nExpected: ~a\nActual: ~a\n"
                   'expr expected actual)))))))

(test 'interval-compare-1
  (interval-compare
    '(#f . #f) '(#f . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'eq)
(test 'interval-compare-2
  (interval-compare
    '(2 . #f) '(#f . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'a-in-b)
(test 'interval-compare-3
  (interval-compare
    '(#f . #f) '(3 . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'b-in-a)
(test 'interval-compare-4
  (interval-compare
    '(#f . 10) '(8 . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt-overlap)
(test 'interval-compare-5
  (interval-compare
    '(#f . 10) '(10 . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)
(test 'interval-compare-6
  (interval-compare
    '(8 . #f) '(#f . 10) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt-overlap)
(test 'interval-compare-7
  (interval-compare
    '(10 . #f) '(#f . 10) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)

(test 'interval-compare-8
  (interval-compare
    '(0 . 100) '(0 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'eq)
(test 'interval-compare-9
  (interval-compare
    '(2 . 100) '(0 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'a-in-b)
(test 'interval-compare-10
  (interval-compare
    '(0 . 100) '(3 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'b-in-a)
(test 'interval-compare-11
  (interval-compare
    '(0 . 10) '(8 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt-overlap)
(test 'interval-compare-12
  (interval-compare
    '(0 . 10) '(10 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)
(test 'interval-compare-13
  (interval-compare
    '(8 . 100) '(0 . 10) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt-overlap)
(test 'interval-compare-14
  (interval-compare
    '(10 . 100) '(0 . 10) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)

(test 'interval-compare-15
  (interval-compare
    1 '(1 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)
(test 'interval-compare-16
  (interval-compare
    2 '(1 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'a-in-b)
(test 'interval-compare-17
  (interval-compare
    100 '(1 . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)
(test 'interval-compare-18
  (interval-compare
    '(1 . 100) 1 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)
(test 'interval-compare-19
  (interval-compare
    '(1 . 100) 99 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'b-in-a)
(test 'interval-compare-20
  (interval-compare
    '(1 . 100) 100 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)

(test 'interval-compare-21
  (interval-compare
    1 '(1 . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)
(test 'interval-compare-22
  (interval-compare
    2 '(1 . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'a-in-b)
(test 'interval-compare-23
  (interval-compare
    2 '(#f . #f) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'a-in-b)
(test 'interval-compare-24
  (interval-compare
    100 '(#f . 100) 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)
(test 'interval-compare-25
  (interval-compare
    '(1 . #f) 1 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'gt)
(test 'interval-compare-26
  (interval-compare
    '(#f . 100) 99 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'b-in-a)
(test 'interval-compare-27
  (interval-compare
    '(#f . #f) 99 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'b-in-a)
(test 'interval-compare-28
  (interval-compare
    '(#f . 100) 100 'lt 'lt-overlap 'a-in-b 'eq 'b-in-a 'gt-overlap 'gt)
  'lt)

(test 'numeric-set-join-1
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . #f)))
(test 'numeric-set-join-2
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 10 100 (100 . 200) 200))
  '((#f . 8) (8 . #f)))
(test 'numeric-set-join-3
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 100 (100 . 200) 200))
  '((#f . 10) (10 . #f)))
(test 'numeric-set-join-4
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 (100 . 200) 200))
  '((#f . 100) (100 . #f)))
(test 'numeric-set-join-5
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200)))
  '((#f . 200) (200 . #f)))
(test 'numeric-set-join-6
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '((-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . -3) (-3 . #f)))
(test 'numeric-set-join-7
  (numeric-set-join '((#f . -3) 5 (7 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . #f)))
(test 'numeric-set-join-8
  (numeric-set-join '((#f . -3) 0 5 (8 . 10) (10 . 100) 101 (200 . #f))
                    '((-5 . 0) 0 (0 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . #f)))
(test 'numeric-set-join-9
  (numeric-set-join '((#f . -3) 5 (8 . 9) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . 9) 10 (10 . #f)))
(test 'numeric-set-join-10
  (numeric-set-join '((#f . -3) 5 (8 . 10) (10 . 100) (200 . 900))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((#f . 900)))
(test 'numeric-set-join-11
  (numeric-set-join '((-50 . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((-50 . #f)))
(test 'numeric-set-join-12
  (numeric-set-join '((-50 . -3) 5 (8 . 10) (10 . 100) (200 . 900))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '((-50 . 900)))

(test 'numeric-set-meet-1
  (numeric-set-meet '((#f . #f))
                    '((#f . #f)))
  '((#f . #f)))
(test 'numeric-set-meet-2
  (numeric-set-meet '((#f . #f))
                    '((#f . 8)))
  '((#f . 8)))
(test 'numeric-set-meet-3
  (numeric-set-meet '((9 . #f))
                    '((#f . #f)))
  '((9 . #f)))
(test 'numeric-set-meet-4
  (numeric-set-meet '((#f . 12))
                    '((7 . #f)))
  '((7 . 12)))
(test 'numeric-set-meet-5
  (numeric-set-meet '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f))
                    '(-3 (-3 . 5) (5 . 8) 8 10 100 (100 . 200) 200))
  '())
(test 'numeric-set-meet-6
  (numeric-set-meet '((-4 . 6))
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '((-4 . -3) 5))
(test 'numeric-set-meet-7
  (numeric-set-meet '(-8 (-4 . 6))
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '(-8 (-4 . -3) 5))
(test 'numeric-set-meet-8
  (numeric-set-meet '(-8 (-4 . 6) 15)
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '(-8 (-4 . -3) 5 15))
(test 'numeric-set-meet-9
  (numeric-set-meet '(-8 (-4 . 6) (15 . 20))
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '(-8 (-4 . -3) 5 (15 . 20)))
(test 'numeric-set-meet-10
  (numeric-set-meet '(-8 (-4 . 6) (15 . 200))
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '(-8 (-4 . -3) 5 (15 . 100)))
(test 'numeric-set-meet-11
  (numeric-set-meet '(-8 (-4 . 6) (15 . 201))
                    '((#f . -3) 5 (8 . 10) (10 . 100) (200 . #f)))
  '(-8 (-4 . -3) 5 (15 . 100) (200 . 201)))
