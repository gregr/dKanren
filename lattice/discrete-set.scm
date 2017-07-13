;; A discrete-set may contain pairs, symbols, strings, numbers, booleans, and
;; the empty list.  For efficiency, we define a total order over these types.
;; Due to the lack of a portable symbol<?, we make use of string<?, which isn't
;; sound for generated symbols.

(define (pair-compare a b lt eq gt)
  ((any-compare (car a) (car b) (lambda () lt)
                (lambda () (any-compare (cdr a) (cdr b) lt eq gt))
                (lambda () gt))))
;; NOTE: this is only sound for non-generated symbols.
(define (symbol-compare a b lt eq gt)
  (cond
    ((eq? a b) eq)
    ((string<? (symbol->string a) (symbol->string b)) lt)
    (else gt)))
(define (string-compare a b lt eq gt)
  (cond ((string<? a b) lt) ((string>? a b) gt) (else eq)))
(define (number-compare a b lt eq gt)
  (cond ((< a b) lt) ((> a b) gt) (else eq)))
(define (boolean-compare a b lt eq gt) (if a (if b eq gt) (if b lt eq)))

;; TODO: auto-generate this.
(define (any-compare a b lt eq gt)
  (if (pair? a) (if (pair? b) (pair-compare a b lt eq gt) lt)
    (if (pair? b) gt
      (if (symbol? a) (if (symbol? b) (symbol-compare a b lt eq gt) lt)
        (if (symbol? b) gt
          (if (string? a) (if (string? b) (string-compare a b lt eq gt) lt)
            (if (string? b) gt
              (if (number? a) (if (number? b) (number-compare a b lt eq gt) lt)
                (if (number? b) gt
                  (if (boolean? a) (if (boolean? b) (boolean-compare a b lt eq gt) lt)
                    (if (boolean? b) gt
                      (if (and (null? a) (null? b)) eq
                        (error 'any-compare
                               (format "unsupported comparison of ~s and ~s"
                                       a b))))))))))))))

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
