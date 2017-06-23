(load "transparent.scm")

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


(define (evalo expr value) (eval-expo expr '() value))
(define-relation (eval-expo expr env value)
  (conde
    ;; Placing lambdas first seems to reduce quoted closures.
    ((fresh (body)
       (== `(lambda ,body) expr)
       (== `(closure ,body ,env) value)))
    ((fresh (datum)
       (== `(quote ,datum) expr)
       (== datum value)))
    ((fresh (a*)
       (== `(list . ,a*) expr)
       (eval-listo a* env value)))
    ((fresh (index)
       (== `(var ,index) expr)
       (lookupo index env value)))
    ((fresh (rator rand arg env^ body)
       (== `(app ,rator ,rand) expr)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `(,arg . ,env^) value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (c va vd)
       (== `(car ,c) expr)
       (== va value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (c va vd)
       (== `(cdr ,c) expr)
       (== vd value)
       (eval-expo c env `(,va . ,vd))))
    ))

;; Avoiding quoted closures in this way is too slow.
;(define-relation (quotedo datum)
  ;(conde
    ;((fresh (a d)
       ;(== `(,a . ,d) datum)
       ;(quotedo a)
       ;(quotedo d)))
    ;((== 'quote datum))
    ;((== 'list datum))
    ;((== 'lambda datum))
    ;((== 'app datum))
    ;((== 'var datum))
    ;((== '() datum))
    ;((== 's datum))))

(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i*)
         (== `(s . ,i*) index)
         (lookupo i* e* value))))))

(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))

;; First order version that avoids quoted closures.
;(define (evalo expr value) (eval-expo expr '() value))
;(define-relation (eval-expo expr env value)
  ;(conde
    ;((fresh (datum)
       ;(== `(quote ,datum) expr)
       ;(== datum value)))
    ;((fresh (a*)
       ;(== `(list . ,a*) expr)
       ;(eval-listo a* env value)))
    ;((fresh (index)
       ;(== `(var ,index) expr)
       ;(lookupo index env value)))
    ;((fresh (rator rand arg body)
       ;(== `(app ,rator ,rand) expr)
       ;(== `(lambda ,body) rator)
       ;(eval-expo rand env arg)
       ;(eval-expo body `(,arg . ,env) value)))))

(time (test 'evalo-1
  (run 1 (q) (evalo q q))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
(time (test 'evalo-step-1
  (car (stream-pretty (step 11811 (query (q) (evalo q q)))))
  '()))
(time (test 'evalo-step-2
  (car (stream-pretty (step 11812 (query (q) (evalo q q)))))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
