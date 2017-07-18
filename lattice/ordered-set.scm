(load "order.scm")

(define (ordered-set-join xs ys)
  (if (null? xs) ys
    (if (null? ys) xs
      ((any-compare
         (car xs) (car ys)
         (lambda () (cons (car xs) (ordered-set-join (cdr xs) ys)))
         (lambda () (cons (car xs) (ordered-set-join (cdr xs) (cdr ys))))
         (lambda () (cons (car ys) (ordered-set-join xs (cdr ys)))))))))
(define (ordered-set-meet xs ys)
  (if (null? xs) '()
    (if (null? ys) '()
      ((any-compare
         (car xs) (car ys)
         (lambda () (ordered-set-meet (cdr xs) ys))
         (lambda () (cons (car xs) (ordered-set-meet (cdr xs) (cdr ys))))
         (lambda () (ordered-set-meet xs (cdr ys))))))))
(define (ordered-set-subtract xs ys)
  (if (null? xs) '()
    (if (null? ys) xs
      ((any-compare
         (car xs) (car ys)
         (lambda () (cons (car xs) (ordered-set-subtract (cdr xs) ys)))
         (lambda () (ordered-set-subtract (cdr xs) (cdr ys)))
         (lambda () (ordered-set-subtract xs (cdr ys))))))))

(define ordered-set-empty '())
(define (ordered-set xs) (merge-sort ordered-set-join xs))
