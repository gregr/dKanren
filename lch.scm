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

(define (list-foldr f acc xs)
  (if (null? xs)
    acc
    (let ((ns (f (car xs) (list-foldr f acc (cdr xs)))))
      (if (and (pair? ns) (pair? (car ns)) (eq? #t (caar ns)))
        (cons (cons #f (cdar ns)) (cdr ns))
        ns))))

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
(define interval-full '(#f . #f))
(define (interval-invert x)
  (cond
    ((equal? interval-full x) '())
    ((number? x) `((#f . ,x) (,x . #f)))
    ((not (car x)) `(,(cdr x) (,(cdr x) . #f)))
    ((not (cdr x)) `((#f . ,(car x)) ,(car x)))
    (else `((#f . ,(car x)) ,(car x) ,(cdr x) (,(cdr x) . #f)))))
(define (interval-overlap-join a b) `(,(car a) . ,(cdr b)))
(define (interval-overlap-meet a b) `(,(car b) . ,(cdr a)))

(define (interval-compare a b
                          lt
                          lt-overlap
                          a-in-b
                          eq
                          b-in-a
                          gt-overlap
                          gt)
  (cond
    ((and (number? a) (number? b))
     (cond
       ((< a b) lt)
       ((> a b) gt)
       (else eq)))
    ((number? a)
     (let ((ba (car b)) (bd (cdr b)))
       (cond
         ((and ba (<= a ba)) lt)
         ((and bd (>= a bd)) gt)
         (else a-in-b))))
    ((number? b)
     (let ((aa (car a)) (ad (cdr a)))
       (cond
         ((and aa (<= b aa)) gt)
         ((and ad (>= b ad)) lt)
         (else b-in-a))))
    (else
      (let ((aa (car a)) (ad (cdr a)) (ba (car b)) (bd (cdr b)))
        (cond
          ((and (eqv? aa ba) (eqv? ad bd)) eq)
          ((number? aa)
           (cond
             ((and bd (>= aa bd)) gt)
             ((number? ad)
              (cond
                ((and ba (<= ad ba)) lt)
                ((and ba (<= aa ba))
                 (cond
                   ((and bd (>= ad bd)) b-in-a)
                   ((and ba (< aa ba)) lt-overlap)
                   (else a-in-b)))
                ((and bd (> ad bd)) gt-overlap)
                (else a-in-b)))
             ((and ba (<= aa ba)) b-in-a)
             ((not bd) a-in-b)
             (else gt-overlap)))
          ((number? ad)
           (cond
             ((and bd (>= ad bd)) b-in-a)
             ((and ba (<= ad ba)) lt)
             ((not ba) a-in-b)
             (else lt-overlap)))
          (else b-in-a))))))

(define numeric-set-empty '())
(define numeric-set-full `(,interval-full))

(define (numeric-set-join a b)
  (define (loop a b)
    (cond
      ((null? a) b)
      ((null? b) a)
      (else
        (let ((ia (car a)) (ib (car b)))
          ((interval-compare
             ia ib
             (lambda ()  ;; lt
               (cons ia (loop (cdr a) b)))
             (lambda ()  ;; lt-overlap
               (loop (cdr a) (cons (interval-overlap-join ia ib) (cdr b))))
             (lambda ()  ;; a-in-b
               (loop (cdr a) b))
             (lambda ()  ;; eq
               (cons ia (loop (cdr a) (cdr b))))
             (lambda ()  ;; b-in-a
               (loop (cdr b) a))
             (lambda ()  ;; gt-overlap
               (loop (cdr b) (cons (interval-overlap-join ib ia) (cdr a))))
             (lambda ()  ;; gt
               (cons ib (loop (cdr b) a)))))))))
  ;; Consolidate remaining adjacent interval-point-intervals.
  ;; (A . n) n (n . B) becomes (A . B)
  (let loop ((ns (loop a b)))
    (cond
      ((null? ns) ns)
      ((and (pair? (cdr ns)) (pair? (cddr ns))
            (pair? (car ns)) (number? (cadr ns)) (pair? (caddr ns)))
       (let ((i0 (car ns)) (i1 (cadr ns)) (i2 (caddr ns)))
         (if (and (= (cdr i0) i1) (= i1 (car i2)))
           (loop (cons (cons (car i0) (cdr i2)) (cdddr ns)))
           (cons i0 (loop (cdr ns))))))
      (else (cons (car ns) (loop (cdr ns)))))))

(define (numeric-set-meet-interval x ns)
  (if (null? ns)
    '()
    (let ((i (car ns)))
      ((interval-compare
         x i
         (lambda () '())                                 ;; lt
         (lambda () (list (interval-overlap-meet x i)))  ;; lt-overlap
         (lambda () (list x))                            ;; a-in-b
         (lambda () (list x))                            ;; eq
         (lambda ()  ;; b-in-a
           (cons i (numeric-set-meet-interval x (cdr ns))))
         (lambda ()  ;; gt-overlap
           (cons (interval-overlap-meet i x)
                 (numeric-set-meet-interval x (cdr ns))))
         (lambda ()  ;; gt
           (numeric-set-meet-interval x (cdr ns))))))))

(define (numeric-set-meet a b)
  (list-foldr
    (lambda (x ns)
      (numeric-set-join (numeric-set-meet-interval x b) ns))
    '()
    a))

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

;; These are only intended as intermediate states for lattices.  Simplification
;; will convert full lattices to top and empty lattices to bottom.
(define type-union-full (type-union top top top top top top))
(define type-union-empty
  (type-union bottom bottom bottom bottom bottom bottom))
(define type-pair-full
  (type-pair top top #t finite-set-empty finite-set-empty))
(define type-pair-empty
  (type-pair bottom bottom #f finite-set-empty finite-set-empty))
(define type-symbol-full (type-symbol #t finite-set-empty))
(define type-symbol-empty (type-symbol #f finite-set-empty))
(define type-number-full
  (type-number #f numeric-set-full finite-set-empty))
(define type-number-empty
  (type-number #f numeric-set-empty finite-set-empty))

(define (simplify nested? a)
  (cond
    ((type-union? a)
     (let ((tu (type-union
                 (simplify #t (tu-pair a))
                 (simplify #t (tu-symbol a))
                 (simplify #t (tu-number a))
                 (tu-nil a)
                 (tu-false a)
                 (tu-true a))))
       (cond
         ((equal? type-union-full tu) top)
         ((equal? type-union-empty tu) bottom)
         (else tu))))
    ((type-pair? a)
     (let ((tp-car (simplify #t (type-pair-car a)))
           (tp-cdr (simplify #t (type-pair-cdr a))))
       (cond
         ((and nested? (equal? type-pair-full a)) top)
         ((or (bottom? tp-car) (bottom? tp-cdr)) bottom)
         (else (type-pair
                 tp-car
                 tp-cdr
                 (type-pair-complement-fd? a)
                 (type-pair-fd a)
                 (type-pair-absents a))))))
    ((type-symbol? a)
     (cond
       ((and nested? (equal? type-symbol-full a)) top)
       ((equal? type-symbol-empty a) bottom)
       (else a)))
    ((type-number? a)
     (cond
       ((and nested? (equal? type-number-full a)) top)
       ((equal? numeric-set-empty (type-number-set a)) bottom)
       (else a)))
    (else a)))
