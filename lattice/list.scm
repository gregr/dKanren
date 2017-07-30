(define (list-foldl f acc xs)
  (if (null? xs)
    acc
    (list-foldl f (f (car xs) acc) (cdr xs))))
(define (list-foldr f acc xs)
  (if (null? xs)
    acc
    (f (car xs) (list-foldr f acc (cdr xs)))))
(define (list-append-map f xs)
  (list-foldr append '() (map eq-linear-solved eqs)))

(define (list-last xs) (if (null? (cdr xs)) (car xs) (list-last (cdr xs))))
