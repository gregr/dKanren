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

(define (list-odds xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else (cons (car xs) (list-odds (cddr xs))))))

(define ordered-set-empty '())
(define (ordered-set xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else (ordered-set-join
            (ordered-set (list-odds xs)) (ordered-set (list-odds (cdr xs)))))))
