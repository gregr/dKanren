(load "transparent-evalo.scm")

(define (q-np n)
  (query (defn)
         (fresh (body)
           (== `(lambda ,body) defn)
           (evalo `(list (app ,defn '(x)) (app ,defn '(y)))
                  `((,n x) (,n y))))))

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

(print-labeled-solution* (q-np 1))

;(print-labeled-solution q-quine)




