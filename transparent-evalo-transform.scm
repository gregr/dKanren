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

(define-relation (list-argso a*)
  (conde
    ((== '() a*))
    ((fresh (a d)
       (== `(,a . ,d) a*)
       (transformo a)
       (list-argso d)))))

(define-relation (transformo e)
  (conde
    ((== '(var ()) e))
    ((fresh (a* a d) (== `(,a . ,d) a*) (== `(list . ,a*) e) (list-argso a*)))
    ((fresh (ec) (== `(car ,ec) e) (refo ec)))
    ((fresh (ec) (== `(cdr ,ec) e) (refo ec)))
    ((fresh (q) (== `(quote ,q) e) (literalo q)))
    ((fresh (ea ed) (== `(cons ,ea ,ed) e) (transformo ea) (transformo ed)))))


(define q-transform-defs
  (query (defn) (fresh (body) (== `(lambda ,body) defn) (transformo body))))

(define MULTIPLIER 5.0)

(define (q-examples n defn)
  (map (lambda (st)
         (caar (run 1 (v)
                 (== (walk* st var-initial) v)
                 (literalo v))))
       (stream-take
         (* n MULTIPLIER)
         (query (input output) (evalo `(app ,defn ',input) output)
                (literalo output)))))

(define (q-examples-exact n defn)
  (map (lambda (st)
         (caar (run 1 (v)
                 (== (walk* st var-initial) v)
                 (literalo v))))
       (stream-take n
         (query (input output) (evalo `(app ,defn ',input) output)
                (literalo output)))))

(define (take-random lst)
  (if (null? lst)
      lst
      (if (<= (random 0.999) (/ 1.0 MULTIPLIER))
          (cons (car lst) (take-random (cdr lst)))
          (take-random (cdr lst)))))

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

(define (use-app n i* proc)
  (if (= 0 n) proc
    (use-app (- n 1) (cdr i*) `(app ,proc ',(car i*)))))

(define (q/hint* code-context code-goal n is os)
  (define q
    (query (target)
           (evalo `(list . ,(map (lambda (i*)
                                   (use-app n i* (code-context target)))
                                 is)) os)))
  (list (== (list code-goal) var-initial) q))

;; We will interact with uses of this.
(define (q-transform/hint* code-context code-goal n inputs)
  (define code-full (code-context code-goal))
  (define uses (map (lambda (i*) (use-app n i* code-full)) inputs))
  (define outputs
    (car (car (run 1 (outputs) (evalo `(list . ,uses) outputs)))))
  (q/hint* code-context code-goal n inputs outputs))
