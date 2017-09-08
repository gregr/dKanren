(load "transparent-evalo.scm")

(define (q-transform f inputs)
  (query (defn)
         (fresh (body)
           (== `(lambda ,body) defn)
           (evalo `(list . ,(map (lambda (input) `(app ,defn ',input)) inputs))
                  (map f inputs)))))

(define (q-transform-hint f inputs hint)
  (query (defn)
         (== hint defn)
         (evalo `(list . ,(map (lambda (input) `(app ,defn ',input)) inputs))
                (map f inputs))))

(define (q-np n) (q-transform (lambda (x) (cons n x)) '((x) (y))))

(define q-quine (query (p) (evalo p p)))

(define (print-labeled-solution q)
  (let loop ((choices (labeled-pretty (labeled-solution q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (if (caar choices) 0 1) (cadar choices))
      (loop (cdr choices)))))

(define (print-labeled-solution* q)
  (define (boolean->idx b) (if b 0 1))
  (let loop ((choices (labeled-pretty (labeled-solution* q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (map boolean->idx (caar choices)) (cadar choices))
      (loop (cdr choices)))))

(define (print-labeled-solution*-hint q-hint q)
  (define (boolean->idx b) (if b 0 1))
  (let loop ((choices (labeled-pretty (labeled-solution*-hint q-hint q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (map boolean->idx (caar choices)) (cadar choices))
      (loop (cdr choices)))))

;(print-labeled-solution* (q-np 1))

;(print-labeled-solution q-quine)


;; list manipulation examples

;; 22 steps
(print-labeled-solution* (q-transform (lambda (x) `(,@x ,@x)) '((a) (b))))

;; 34 steps (takes a moment to synthesize)
;(print-labeled-solution* (q-transform (lambda (x) `(,@x 3 ,@x)) '((a) (b))))

;; 26 steps
;(print-labeled-solution*-hint
  ;(q-transform-hint
    ;(lambda (x) (cons (car x) (cddr x)))
    ;'((a b c) (d e f))
    ;'(lambda (cons (car (var ())) (cdr (cdr (var ()))))))
  ;(q-transform
    ;(lambda (x) (cons (car x) (cddr x)))
    ;'((a b c) (d e f))))
