(load "transparent-evalo-transform.scm")

(define (print-labeled-solution*-hint q-hint q)
  (define (boolean->idx b) (if b 0 1))
  (let loop ((choices (labeled-pretty (labeled-solution*-hint q-hint q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (map boolean->idx (caar choices)) (cadar choices))
      (loop (cdr choices)))))


(define (print x) (printf "~s\n" x))

(define (print-example-solutions danswer)
  (define lam (car danswer))
  (define ios (q-examples 4 lam))
  (define is (map car ios))
  (define os (map cadr ios))
  (define qh (q/hint lam is os))
  (define q-hint (car qh))
  (define q (cadr qh))

  ;; Uncomment some of these to see the kinds of examples we're generating.
  (print `(q-transform/hint ',lam ',is))

  ;(print `(lam ,lam))
  ;(print `(ios ,ios))
  ;(print `(inputs ,is))
  ;(print `(outputs ,os))
  ;(print `(with hint ,(reify-initial (car (stream-take 1 q-hint)))))
  ;(print `(without hint ,(reify-initial (car (stream-take 1 q)))))
  ;(print `(lam ,lam ios ,is ,os)) ; PROBLEM

  ;; Comment this if you uncomment anything above.
  ;(print-labeled-solution*-hint q-hint q)
  )

(define (print-example-solutions/map danswer)
  (define (process ios)
    (define ioss
      (list (list (car ios))
            (list (cadr ios) (caddr ios))
            (list (cadddr ios) (cadddr (cdr ios)) (cadddr (cddr ios)))))
    (define iss (map (lambda (ios) (list (map car ios))) ioss))
    (define oss (map (lambda (ios) (map cdr ios)) ioss))

    (print `(q-transform/hint* Z-map-context '(app (var ()) ,code) 1 ',iss))

    ;(print `(code ,code))
    ;(print `(ios ,ios))
    ;(print `(inputs ,is))
    ;(print `(outputs ,os))
    ;(print `(with hint ,(reify-initial (car (stream-take 1 hint)))))
    ;(print `(without hint ,(reify-initial (car (stream-take 1 q)))))
    ;(print `(code ,code ios ,iss ,oss)) ; PROBLEM
    )
  (define code (car danswer))
  (define ios (q-examples-exact 6 code))
  ;(displayln `(ios: ,ios code: ,code))
  (when (= 6 (length ios)) (process ios)))

(define (stream-process n k q)
  (let loop ((n n) (q q))
    (cond
      ((or (and n (= 0 n)) (null? q)) #f)
      ((pair? q) (k (reify-initial (car q))) (loop (and n (- n 1)) (cdr q)))
      (else (loop n (stream-next q))))))

;; Change the first argument to #f to generate indefinitely.
(stream-process #f print-example-solutions/map q-transform-defs)
