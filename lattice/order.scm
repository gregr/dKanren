(define (vector-compare a b lt eq gt)
  (define len-a (vector-length a))
  (define len-b (vector-length b))
  (cond
    ((< len-a len-b) lt)
    ((> len-a len-b) gt)
    (else (pair-compare (vector->list a) (vector->list b) lt eq gt))))
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
  (if (vector? a) (if (vector? b) (vector-compare a b lt eq gt) lt)
   (if (vector? b) gt
     (if (pair? a) (if (pair? b) (pair-compare a b lt eq gt) lt)
       (if (pair? b) gt
         (if (symbol? a) (if (symbol? b) (symbol-compare a b lt eq gt) lt)
           (if (symbol? b) gt
             (if (string? a) (if (string? b) (string-compare a b lt eq gt) lt)
               (if (string? b) gt
                 (if (number? a)
                   (if (number? b) (number-compare a b lt eq gt) lt)
                   (if (number? b) gt
                     (if (boolean? a)
                       (if (boolean? b) (boolean-compare a b lt eq gt) lt)
                       (if (boolean? b) gt
                         (if (and (null? a) (null? b)) eq
                           (error 'any-compare
                                  (format "unsupported comparison of ~s and ~s"
                                          a b))))))))))))))))

(define (any-max xs)
  (let loop ((x (car xs)) (xs (cdr xs)))
    (if (null? xs) x
      (loop (any-compare x (car xs) (car xs) x x) (cdr xs)))))
(define (any-min xs)
  (let loop ((x (car xs)) (xs (cdr xs)))
    (if (null? xs) x
      (loop (any-compare x (car xs) x x (car xs)) (cdr xs)))))

(define (list-odds xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else (cons (car xs) (list-odds (cddr xs))))))
(define (merge-sort merge xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) xs)
    (else (merge (merge-sort merge (list-odds xs))
                 (merge-sort merge (list-odds (cdr xs)))))))
