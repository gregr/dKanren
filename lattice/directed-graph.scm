(load "dict.scm")
(load "ordered-set.scm")

(define dg-empty dict-empty)

(define (dg-add dg a b)
  (define (merge k xs ys) (ordered-set-join xs ys))
  (dict-join id-value id-value merge dg (dict `((,a . (,b))))))

(define (dg-remove dg a b)
  (define succ (dg-succ dg a))
  (if (ordered-set-member? succ b)
    (let ((succ (ordered-set-subtract succ (ordered-set-singleton b))))
      (if (null? succ) (dict-remove dg a) (dict-set dg a succ)))
    dg))

(define (dg-succ dg x)
  (define proj (dict-project dg (list x)))
  (if (null? proj) '() (cdar proj)))

(define (dg-pred dg x)
  (map car (dict-filter (lambda (xs) (ordered-set-member? xs x)) dg)))

(define (dg-tc dg r x)
  (define immediate (r dg x))
  (if (null? immediate) '()
    (ordered-set-join
      immediate (ordered-set-join-map
                  (lambda (y) (dg-tc dg r y)) immediate))))

(define (dg-succ* dg x) (dg-tc dg dg-succ x))
(define (dg-pred* dg x) (dg-tc dg dg-pred x))
