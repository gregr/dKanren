(load "dict.scm")
(load "ordered-set.scm")

(define dg-empty dict-empty)

(define (dg-add dg a b)
  (define (merge k xs ys) (ordered-set-join xs ys))
  (dict-join id-value id-value merge dg (dict `((,a . (,b))))))

(define (dg-succ dg x)
  (define proj (dict-project dg (list x)))
  (if (null? proj) '() (cdar proj)))

(define (dg-pred dg x)
  (map car (dict-filter (lambda (xs) (ordered-set-member? xs x)) dg)))
