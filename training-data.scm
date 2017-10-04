(load "transparent-evalo.scm")

(define-relation (atomo v)
  (conde
    ((== '() v)) ((== #t v)) ((== #f v)) ((== 'a v)) ((== 'b v)) ((== 's v))
    ((== '1 v)) ((== 'x v)) ((== 'y v)) ((== 'quote v)) ((== 'list v))
    ((== 'cons v)) ((== 'car v)) ((== 'cdr v)) ((== 'var v)) ((== 'lambda v))
    ((== 'app v)) ((== 'closure v))))

(define-relation (literalo v)
  (conde
    ((atomo v))
    ((fresh (a d) (== `(,a . ,d) v) (literalo a) (literalo d)))))

(define-relation (refo e)
  (conde
    ((== `(var ()) e))
    ((fresh (ec) (== `(car ,ec) e) (refo ec)))
    ((fresh (ec) (== `(cdr ,ec) e) (refo ec)))))

(define-relation (transformo e)
  (conde
    ((== '(var ()) e))
    ((fresh (ea ed) (== `(cons ,ea ,ed) e) (transformo ea) (transformo ed)))
    ((fresh (ec) (== `(car ,ec) e) (refo ec)))
    ((fresh (ec) (== `(cdr ,ec) e) (refo ec)))
    ((fresh (q) (== `(quote ,q) e) (literalo q)))))

(define q-defs
  (query (defn) (fresh (body) (== `(lambda ,body) defn) (transformo body))))

(define (q-examples n defn)
  (run n (input output) (evalo `(app ,defn ',input) output) (literalo input)))

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

  (define q-hint
    (query (defn)
      (== lam defn)
      (evalo `(list . ,(map (lambda (i) `(app ,defn ',i)) is)) os)))
  (define q
    (query (defn)
      (fresh (body) (== `(lambda ,body) defn)
        (evalo `(list . ,(map (lambda (i) `(app ,defn ',i)) is)) os))))

  ;; Uncomment some of these to see the kinds of examples we're generating.
  ;(print `(lam ,lam))
  ;(print `(ios ,ios))
  ;(print `(inputs ,is))
  ;(print `(outputs ,os))
  ;(print `(with hint ,(reify-initial (car (stream-take 1 q-hint)))))
  ;(print `(without hint ,(reify-initial (car (stream-take 1 q)))))
  ;(print `(lam ,lam ios ,is ,os))

  ;; Comment this if you uncomment anything above.
  (print-labeled-solution*-hint q-hint q)
  )

(define (stream-process n k q)
  (let loop ((n n) (q q))
    (cond
      ((or (and n (= 0 n)) (null? q)) #f)
      ((pair? q) (k (reify-initial (car q))) (loop (and n (- n 1)) (cdr q)))
      (else (loop n (stream-next q))))))

;; Change the first argument to #f to generate indefinitely.
(stream-process 10 print-example-solutions q-defs)
