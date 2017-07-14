(load "ordered-set.scm")

;; A discrete-set may contain:
;;   vectors, pairs, symbols, strings, numbers, booleans, the empty list
;; For efficiency, we define a total order over these types. Due to the lack of
;; a portable symbol<?, we make use of string<?, which isn't sound for
;; generated symbols (see ordered-set.scm for details).

(define (discrete-set complement? items) (cons complement? items))
(define (discrete-set-complement? ds) (car ds))
(define (discrete-set-items ds) (cdr ds))
(define (discrete-set-with xs) (discrete-set #f (ordered-set xs)))
(define (discrete-set-without xs) (discrete-set #t (ordered-set xs)))
(define discrete-set-empty (discrete-set-with '()))
(define discrete-set-full (discrete-set-without '()))

(define (discrete-set-complement ds)
  (discrete-set (not (discrete-set-complement? ds)) (discrete-set-items ds)))

(define (discrete-set-join-items parity complement-a? complement-b? a b)
  (if complement-a?
    (if complement-b?
      (discrete-set parity (ordered-set-meet a b))
      (discrete-set parity (ordered-set-subtract a b)))
    (if complement-b?
      (discrete-set parity (ordered-set-subtract b a))
      (discrete-set (not parity) (ordered-set-join a b)))))

(define (discrete-set-join a b)
  (discrete-set-join-items
    #t
    (discrete-set-complement? a)
    (discrete-set-complement? b)
    (discrete-set-items a)
    (discrete-set-items b)))

(define (discrete-set-meet a b)
  (discrete-set-join-items
    #f
    (not (discrete-set-complement? a))
    (not (discrete-set-complement? b))
    (discrete-set-items a)
    (discrete-set-items b)))
