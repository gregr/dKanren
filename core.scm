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

;; TODO: hash
(define store-empty '())
(define (store-ref store key . default)
  (let ((binding (assoc key store)))
    (if binding
      (cdr binding)
      (if (null? default)
        (error 'store-ref (format "missing key ~s in ~s" key store))
        (car default)))))
(define (store-set store key value) `((,key . ,value) . ,store))
(define (store-remove store key)
  (if (null? store)
    '()
    (if (eqv? key (caar store))
      (store-remove (cdr store) key)
      (cons (car store) (store-remove (cdr store) key)))))
(define (store-keys store) (map car store))

(define (list-add-unique xs v) (if (member v xs) xs (cons v xs)))
(define (list-append-unique xs ys)
  (if (null? xs)
    ys
    (let ((zs (list-append-unique (cdr xs) ys))
          (x0 (car xs)))
      (if (member x0 ys) zs (cons x0 zs)))))
(define (list-remove-unique xs v)
  (cond
    ((null? xs) '())
    ((equal? v (car xs)) (cdr xs))
    (else (let ((xs1 (list-remove-unique (cdr xs))))
            (if (eq? xs1 (cdr xs))
              xs
              (cons (car xs) xs1))))))
(define (list-subtract xs ys)
  (if (null? xs)
    '()
    (let ((x0 (car xs))
          (xs1 (list-subtract (cdr xs) ys)))
     (if (member x0 ys)
       xs1
       (if (eq? xs1 (cdr xs))
         xs
         (cons x0 xs1))))))
(define (list-overlap? xs ys)
  (and (pair? xs)
       (or (member (car xs) ys))
       (list-overlap? (cdr xs) ys)))
(define (list-intersect xs ys)
  (define rest (list-intersect (cdr xs) ys))
  (if (member (car xs) ys)
    (cons (car xs) rest)
    rest))

(define (value-type-tag value)
  (cond
    ((pair? value) 'pair)
    ((symbol? value) 'symbol)
    ((number? value) 'number)
    (else value)))
;; Finite domain lists only for symbols, numbers: (), #f, #t are singletons.
;; Finite domains are *not* supported for pairs.
;; #t means list is negative.  #f means list represents a finite domain.
(define (domain-type-full tag) `(,tag #t ()))
(define domain-full (map domain-type-full '(pair symbol number () #f #t)))
(define (domain-remove dmn type)
  (cond
    ((null? dmn) '())
    ((eqv? type (caar dmn)) (cdr dmn))
    (else (let ((dmn-new (domain-remove (cdr dmn) type)))
            (if (eq? (cdr dmn) dmn-new)
              dmn
              (cons (car dmn) dmn-new))))))
(define (domain-remove-except dmn type)
  (cond
    ((null? dmn) '())
    ((eqv? type (caar dmn)) (list (car dmn)))
    (else (domain-remove-except (cdr dmn) type))))
(define (domain-type-=/= dt type value)
  (define =/=? (cadr dt))
  (define fd (caddr dt))
  (define (finite fd) (and (pair? fd) `(,type #f ,fd)))
  (if =/=?
    `(,type #t ,(list-add-unique fd value))
    (finite (list-remove-unique fd value))))
(define (domain-=/= dmn value)
  (let ((type (value-type-tag value)))
    (let loop ((dmn dmn))
      (if (eqv? type (caar dmn))
        (let ((dt (domain-type-=/= (car dmn) type value)))
          (if dt
            (cons dt (cdr dmn))
            (cdr dmn)))
        (cons (car dmn) (loop (cdr dmn)))))))
(define (domain-=/=-except dmn fd)
  (let loop ((dlimit '()) (fd fd))
    (if (null? fd)
      (domain-intersect dmn dlimit)
      (loop (domain-add dlimit (car fd)) (cdr fd)))))

;; Pairs must *not* be added.  They do not support finite domains.
(define (domain-type-add dt value)
  (define type (car dt))
  (if (or (symbol? value) (number? value))
    `(,type #f ,(list-add-unique (caddr dt) value))
    `(,type #t ())))
(define (domain-type-set dmn dt)
  (define type (car dt))
  (let loop ((dmn dmn) (full domain-full))
    (cond
      ((eqv? type (caar dmn)) (cons dt (cdr dmn)))
      ((eqv? type (caar full)) (cons dt dmn))
      ((eqv? (caar full) (caar dmn)) (loop (cdr dmn) (cdr full)))
      (else (loop dmn (cdr full))))))
(define (domain-add dmn value)
  (let ((dt (or (domain-type-ref dmn (value-type-tag value)) `(,type #f ()))))
    (domain-type-set dmn (domain-type-add dt value))))
(define (domain-type-ref dmn type)
  (cond
    ((null? dmn) #f)
    ((eqv? type (caar dmn)) (car dmn))
    (else (domain-type-ref (cdr dmn) type))))
(define (domain-has-type? dmn type)
  (or (eq? domain-full dmn) (domain-type-ref dmn type)))
(define (domain-has-value? dmn value)
  (define (domain-type-has-value? dmn type value)
    (define dt (domain-type-ref dmn type))
    (and dt (let ((=/=? (cadr dt)) (present? (member value (caddr dt))))
              (if =/=? (not present?) present?))))
  (or (eq? domain-full dmn)
      (domain-type-has-value? dmn (value-type-tag value) value)))

(define (domain-type-overlap? dt1 dt2)
  (define =/=1? (cadr dt1))
  (define =/=2? (cadr dt2))
  (define fd1 (caddr dt1))
  (define fd2 (caddr dt2))
  (or (and =/=1? =/=2?)
      (cond
        (=/=1? (pair? (list-subtract fd2 fd1)))
        (=/=2? (pair? (list-subtract fd1 fd2)))
        (else (list-overlap? fd1 fd2)))))
(define (domain-overlap? d1 d2)
  (or (eq? domain-full d1) (eq? domain-full d2)
      (let loop ((d1 d1) (d2 d2) (full domain-full))
        (cond
          ((or (null? d1) (null? d2)) #f)
          ((eqv? (caar d1) (caar d2))
           (or (domain-type-overlap? (car d1) (car d2))
               (loop (cdr d1) (cdr d2) full)))
          ((eqv? (caar full) (caar d1)) (loop (cdr d1) d2 (cdr full)))
          ((eqv? (caar full) (caar d2)) (loop d1 (cdr d2) (cdr full)))
          (else (loop d1 d2 (cdr full)))))))

(define (domain-type-intersect dt1 dt2)
  (define tag (car dt1))
  (define =/=1? (cadr dt1))
  (define =/=2? (cadr dt2))
  (define fd1 (caddr dt1))
  (define fd2 (caddr dt2))
  (define (finite fd) (and (pair? fd) `(,tag #f ,fd)))
  (cond
    ((and =/=1? =/=2?) `(,tag #t ,(list-append-unique fd1 fd2)))
    (=/=1? (finite (list-subtract fd2 fd1)))
    (=/=2? (finite (list-subtract fd1 fd2)))
    (else (finite (list-intersect fd1 fd2)))))
(define (domain-intersect d1 d2)
  (cond ((eq? domain-full d1) d2)
        ((eq? domain-full d2) d1)
        (else (let loop ((d1 d1) (d2 d2) (full domain-full))
                (cond
                  ((or (null? d1) (null? d2)) '())
                  ((eqv? (caar d1) (caar d2))
                   (let ((di (domain-type-intersect (car d1) (car d2)))
                         (dis (loop (cdr d1) (cdr d2) full)))
                     (if di (cons di dis) dis)))
                  ((eqv? (caar full) (caar d1)) (loop (cdr d1) d2 (cdr full)))
                  ((eqv? (caar full) (caar d2)) (loop d1 (cdr d2) (cdr full)))
                  (else (loop d1 d2 (cdr full))))))))