;; Lattice-based constraint handling

;; Approximate orthogonality:
;; It should be possible to plug in new lattice components without
;; invalidating existing rules, but those rules may be incomplete on their own
;; with respect to new components (i.e., too conservative, not noticing some
;; new unsatisfiable situations).  Specifying additional "glue" rules to cover
;; new combinations should help reduce new sources of incompleteness.

;; Supported typed lattices:
;; * bottom: nothing, represents failure
;; * singleton: #t, #f, ()
;; * symbol:
;;   finite complement domain (fcd)
;;   > finite domain
;;   > singleton
;; * number:
;;   int? + numeric-set + arithmetic
;;   > singleton
;; * pair:
;;   car, cdr sub-lattices + finite complement shape domain + absents
;;   > car, cdr sub-lattices + finite shape domain
;;   > singleton + released finite [complement] shape constraints
;; * type-union: join of typed lattices
;; * top: anything

;; Supported constraints:
;; ==, =/=, typeo, integero, +o, *o, <=o

;; Finite control operators:
;; finite-and, finite-or, finite-xor, finite-not
;; Behind the scenes, these will meet, join, and complement the lattices
;; involved.  In some cases, this works well enough to eliminate the need for
;; search.  Even when search is necessary, approximate constraints can be given
;; and applied deterministically.  Since the purpose is to notice ASAP when
;; constraints become unsatisfiable, and not to provide a generative model,
;; search can be lazy, stopping at the first instance of satisfaction.  Ideally
;; we would use watched variables to trigger resumption of satisfiability
;; checking only on demand.

;; These constraints can be expressed with finite control operators:
;; (not-betweeno x a b): (finite-xor (<=o x a) (<=o b x))
;; (withino x a b):      (finite-and (<=o a x) (<=o x b))

;; Worries:
;; Is this going to end up gravitating towards being a general SMT solver?

(define-syntax defrecord
  (syntax-rules ()
    ((_ name name?)
     (begin
       (define name (vector 'name))
       (define (name? datum) (eq? name datum))))
    ((_ name name? (field set-field) ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (define (define-field-setter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum value)
                    (let ((new (vector-copy datum)))
                      (vector-set! new ,idx value)
                      new))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...))
         (let ((set-fns (range-assoc 1 '(set-field ...))))
           (begin (define-field-setter 'set-field set-fns) ...)))))
    ((_ name name? field ...)
     (begin
       (define (name field ...) (vector 'name field ...))
       (define (name? datum)
         (and (vector? datum) (eq? 'name (vector-ref datum 0))))
       (let ()
         (define (range-assoc start xs)
           (let loop ((xs xs) (idx start))
             (if (null? xs)
               '()
               (cons (cons (car xs) idx) (loop (cdr xs) (+ idx 1))))))
         (define (define-field-getter name rassc)
           (define idx (cdr (assoc name rassc)))
           (eval `(define (,name datum) (vector-ref datum ,idx))))
         (let ((fns (range-assoc 1 '(field ...))))
           (begin (define-field-getter 'field fns) ...)))))))

(define finite-set-empty '())

;; A numeric-set stores sorted, open intervals of the form (lb . ub) where lb
;; is the lower bound and ub is the upper bound.  The bounds may be #f,
;; representing negative and positive infinity, depending on which side they
;; are placed.  Individual numbers are stored sorted alongside the intervals,
;; and these represent included points.  e.g., the set (2 (4 . 8) 8 (10 . f))
;; represents a union of the point or the closed interval [2 2] with the
;; half-closed interval (4 8] and the open interval (10 +infinity), i.e., all
;; numbers x such that either x = 2 OR 4 < x <= 8 OR 10 < x).
(define numeric-set-empty '())
(define numeric-set-full '((#f . #f)))

(define top #t)
(define (top? a) (eq? top a))
(defrecord type-union type-union?
           tu-pair tu-symbol tu-number tu-nil tu-false tu-true)
(defrecord type-pair type-pair?
           type-pair-car
           type-pair-cdr
           type-pair-complement-fd?
           type-pair-fd
           type-pair-absents)
(defrecord type-symbol type-symbol?
           type-symbol-complement-fd?
           type-symbol-fd)
(defrecord type-number type-number?
           type-number-integer?
           type-number-set
           type-number-arithmetic)
(defrecord singleton singleton? singleton-value)
(define bottom #f)
(define bottom? not)
