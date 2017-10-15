(load "transparent.scm")

(define (atomo v)
  (conde
    ((== '() v)) ((== #t v)) ((== #f v)) ((== 'a v)) ((== 'b v)) ((== 's v))
    ((== '1 v)) ((== 'x v)) ((== 'y v)) ((== 'quote v)) ((== 'list v))
    ((== 'cons v)) ((== 'car v)) ((== 'cdr v)) ((== 'var v)) ((== 'lambda v))
    ((== 'app v)) ((== 'closure v)) ((== 'pair? v)) ((== 'if v))))
(define (not-falseo v)
  (conde
    ((== '() v)) ((== #t v)) ((== 'a v)) ((== 'b v)) ((== 's v))
    ((== '1 v)) ((== 'x v)) ((== 'y v)) ((== 'quote v)) ((== 'list v))
    ((== 'cons v)) ((== 'car v)) ((== 'cdr v)) ((== 'var v)) ((== 'lambda v))
    ((== 'app v)) ((== 'closure v)) ((== 'pair? v)) ((== 'if v))
    ((fresh (a d) (== `(,a . ,d) v)))))

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
    ((fresh (e a d v)
       (== `(pair? ,e) expr)
       (conde
         ((== #t value) (eval-expo e env `(,a . ,d)))
         ((== #f value) (eval-expo e env v) (atomo v)))))
    ((fresh (c t f v)
       (== `(if ,c ,t ,f) expr)
       (conde
         ((eval-expo c env #f) (eval-expo f env value))
         ((eval-expo c env v) (not-falseo v) (eval-expo t env value)))))))

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

;(define Z0
  ;(lambda (f)
    ;((lambda (x) (lambda (a) ((f (x x)) a)))
     ;(lambda (x) (lambda (a) ((f (x x)) a))))))

(define Z
  '(lambda
     (app (lambda (lambda (app (app (var (s s))
                                    (app (var (s)) (var (s)))) (var ()))))
          (lambda (lambda (app (app (var (s s))
                                    (app (var (s)) (var (s)))) (var ())))))))

;(define Z0-append
  ;(Z0 (lambda (append)
    ;(lambda (xs)
      ;(lambda (ys)
        ;(if (pair? xs)
          ;(cons (car xs) ((append (cdr xs)) ys))
          ;ys))))))

(define Z-append
  `(app ,Z (lambda
             (lambda
               (lambda
                 (if (pair? (var (s)))
                   (cons (car (var (s)))
                         (app (app (var (s s)) (cdr (var (s)))) (var ())))
                   (var ())))))))

;(define Z0-map
  ;(Z0 (lambda (map)
       ;(lambda (f)
         ;(lambda (xs)
           ;(if (pair? xs)
             ;(cons (f (car xs)) ((map f) (cdr xs)))
             ;'()))))))

(define Z-map
  `(app ,Z (lambda
             (lambda
               (lambda
                 (if (pair? (var ()))
                   (cons (app (var (s)) (car (var ())))
                         (app (app (var (s s)) (var (s))) (cdr (var ()))))
                   '()))))))
