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

(define (dg-add-simplify dg a b)
  ;; TODO: recognize intermediate simplification.
  (define (err . _) (error 'dg-add-simplify "This should not happen."))
  (define (rem k x _) (ordered-set-subtract x (ordered-set-singleton b)))
  (if (ordered-set-member? (dg-succ* dg a) b) dg
    (let* ((pred (dg-pred dg b))
           (pred (ordered-set-meet pred (dg-pred* dg a)))
           (dpred (map (lambda (k) (cons k #t)) pred))
           (dg (dict-join id-value err rem dg dpred)))
      (dg-add dg a b))))

(define (dg-replace dg xs y)
  (define (replace vs)
    (if (ormap (lambda (x) (ordered-set-member? vs x)) xs)
      (ordered-set-join (ordered-set-subtract vs xs)
                        (ordered-set-singleton y))
      vs))
  (define succ (ordered-set-subtract
                 (ordered-set-join-map cdr (dict-project dg xs)) xs))
  (define removed (dict-subtract dg xs))
  (dict-set (dict-map replace removed) y succ))

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

(define (dg-scc dg a b)
  (if (equal? a b) (ordered-set-singleton b)
    (let ((scc (ordered-set-join-map (lambda (y) (dg-scc dg a y))
                                     (dg-succ dg b))))
      (if (null? scc) '() (ordered-set-join scc (ordered-set-singleton b))))))
