(load "transparent-evalo.scm")

(define-relation (atomo v)
  (conde
    ((== '() v)) ((== #t v)) ((== #f v)) ((== 'a v)) ((== 'b v)) ((== 's v))
    ((== '1 v)) ((== 'x v)) ((== 'y v))

    ;; Leave these out for now.
    ;((== 'quote v)) ((== 'list v))
    ;((== 'cons v)) ((== 'car v)) ((== 'cdr v)) ((== 'var v)) ((== 'lambda v))
    ;((== 'app v))

    ;; Leave this one out, as it can be dangerous.
    ;((== 'closure v))
    ))

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
  (define q-hint
    (query (defn)
      (== fcode defn)
      (evalo `(list . ,(map (lambda (i) `(app ,defn ',i)) is)) os)))
  (define q
    (query (defn)
      (fresh (body) (== `(lambda ,body) defn)
        (evalo `(list . ,(map (lambda (i) `(app ,defn ',i)) is)) os))))
  (list q-hint q))

;; We will interact with uses of this.
(define (q-transform/hint fcode inputs)
  (define outputs
    (car (car (run 1 (outputs)
                (evalo `(list . ,(map (lambda (i) `(app ,fcode ',i))
                                      inputs))
                       outputs)))))
  (q/hint fcode inputs outputs))
