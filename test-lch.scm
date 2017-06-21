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
