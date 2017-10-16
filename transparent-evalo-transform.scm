(load "transparent-evalo.scm")

(define atoms (run* (a) (atomo a)))
(define (atom-random) (list-ref atoms (random (length atoms))))

(define-relation (literalo v)
  (conde
    ((== (atom-random) v))
    ((fresh (a d) (== `(,a . ,d) v) (literalo a) (literalo d)))
    ((atomo v))))

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
  (map (lambda (st)
         (caar (run 1 (v)
                 (== (walk* st var-initial) v)
                 (literalo v))))
       (stream-take n (query (input output) (evalo `(app ,defn ',input) output)
                             (literalo output)))))

(define (q/hint fcode is os)
  (define q
    (query (defn)
      (fresh (body) (== `(lambda ,body) defn)
        (evalo `(list . ,(map (lambda (i) `(app ,defn ',i)) is)) os))))
  (list (== (list fcode) var-initial) q))

;; We will interact with uses of this.
(define (q-transform/hint fcode inputs)
  (define outputs
    (car (car (run 1 (outputs)
                (evalo `(list . ,(map (lambda (i) `(app ,fcode ',i))
                                      inputs))
                       outputs)))))
  (q/hint fcode inputs outputs))
