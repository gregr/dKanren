(load "transparent.scm")

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
       (eval-expo c env `(,va . ,vd))))))

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
