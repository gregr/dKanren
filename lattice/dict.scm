(load "order.scm")

(define (dict-join fx fy fxy xs ys)
  (if (null? xs) ys
    (if (null? ys) xs
      ((any-compare
         (caar xs) (caar ys)
         (lambda () (cons (cons (caar xs) (fx (caar xs) (cdar xs)))
                          (dict-join fx fy fxy (cdr xs) ys)))
         (lambda () (cons (cons (caar xs) (fxy (caar xs) (cdar xs) (cdar ys)))
                          (dict-join fx fy fxy (cdr xs) (cdr ys))))
         (lambda () (cons (cons (caar ys) (fy (caar ys) (cdar ys)))
                          (dict-join fx fy fxy xs (cdr ys)))))))))
(define (dict-meet fxy xs ys)
  (if (null? xs) '()
    (if (null? ys) '()
      ((any-compare
         (caar xs) (caar ys)
         (lambda () (dict-meet fxy (cdr xs) ys))
         (lambda () (cons (cons (caar xs) (fxy (caar xs) (cdar xs) (cdar ys)))
                          (dict-meet fxy (cdr xs) (cdr ys))))
         (lambda () (dict-meet fxy xs (cdr ys))))))))
(define (dict-subtract xs ys)  ;; ys is an ordered-set
  (if (null? xs) '()
    (if (null? ys) xs
      ((any-compare
         (caar xs) (car ys)
         (lambda () (cons (car xs) (dict-subtract (cdr xs) ys)))
         (lambda () (dict-subtract (cdr xs) (cdr ys)))
         (lambda () (dict-subtract xs (cdr ys))))))))
(define (dict-project xs ys)  ;; ys is an ordered-set
  (define (id-left k x y) x)
  (dict-meet id-left xs (map (lambda (k) (cons k #f)) ys)))

(define (dict-filter f xs)
  (cond
    ((null? xs) '())
    ((f (cdar xs)) (cons (car xs) (dict-filter f (cdr xs))))
    (else (dict-filter f (cdr xs)))))
(define (dict-map f xs)
  (map (lambda (kv) (cons (car kv) (f (cdr kv)))) xs))

(define (assoc->dict fx fy fxy xs)
  (merge-sort (lambda (a b) (dict-join fx fy fxy a b)) xs))

(define (id-value k x) x)
(define (id-value-old k x y) x)
(define (id-value-new k x y) y)

(define dict-empty '())
(define (dict xs)
  (define (error-not-unique k x y)
    (error 'dict
           (format "dict given multiple values for the same key: ~s ~s ~s"
                   k x y)))
  (assoc->dict id-value id-value error-not-unique xs))

(define (dict-remove xs k) (dict-subtract xs (ordered-set-singleton k)))
(define (dict-set xs k v)
  (dict-join id-value id-value id-value-new xs (dict `((,k . ,v)))))
