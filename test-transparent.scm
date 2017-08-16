(load "transparent-evalo.scm")

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

(define-relation (appendo l s ls)
  (conde
    ((== '() l) (== s ls))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) ls)
       (appendo d s res)))))

(test 'appendo-1
  (run* (q) (appendo '(a b c) '(d e) q))
  '(((a b c d e))))
(test 'appendo-2
  (run* (q) (appendo '(a b c) q '(a b c d e)))
  '(((d e))))
(test 'appendo-3
  (run* (q) (appendo q '(d e) '(a b c d e)))
  '(((a b c))))
(test 'appendo-4
  (run* (p q) (appendo p q '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))


(time (test 'evalo-1
  (run 1 (q) (evalo q q))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
(time (test 'evalo-step-1
  (car (stream-pretty (step 11811 (query (q) (evalo q q)))))
  '()))
(time (test 'evalo-step-2
  (car (stream-pretty (step 11813 (query (q) (evalo q q)))))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
