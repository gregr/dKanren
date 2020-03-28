(define (finite-set-map f xs)
  )
(define (finite-set-map-join f xs)
  )
(define (compose2 f g) (lambda (x) (f (g x))))

(define (complement a)
  (cond
    ((bottom? a) top)
    ((top? a) bottom)
    ((type-union? a) (type-union (complement (tu-pair a))
                                 (complement (tu-symbol a))
                                 (complement (tu-number a))
                                 (complement (tu-nil a))
                                 (complement (tu-false a))
                                 (complement (tu-true a))))

    ;; TODO: all individual types become type unions:
    ;; the complement of a specific type includes all the other types!

    ((type-pair? a)
     (if (not (equal? finite-set-empty (type-pair-absents a)))
       (error 'complement
              (format "complement does not support pair absence yet ~s" a))
       (let* ((complement-fd? (not (type-pair-complement-fd? a)))
              (fd (type-pair-fd a))
              ;; TODO: need to look up variable lattices when (not complement-fd?).
              ;; Otherwise, ignore them.
              ;; Actually, when complement-fd?, car and cdr aren't really constrained.
              (unit (if complement-fd?
                      join complemented singletons
                      (lambda (v) (complement (singleton v)))
                      singleton)))
         (type-pair
           (finite-set-map-join (compose2 unit car) fd)
           (finite-set-map-join (compose2 unit cdr) fd)
           complement-fd?
           fd
           finite-set-empty))))

    ((type-symbol? a)
     (type-symbol (not (type-symbol-complement-fd?)) (type-symbol-fd a)))
    ((type-number? a)
     (cond
       ((type-number-integer? a)
        (error 'complement
               (format "complement of integer set is not supported yet ~s" a)))
       ((not (equal? finite-set-empty (type-number-arithmetic a)))
        (error 'complement
               (format "complement does not support arithmetic yet ~s" a)))
       (else (type-number
               #f
               (numeric-set-complement (type-number-set a))
               finite-set-empty))))

    ((singleton? a)
     ;; Subtract from the proper type in type-union-full.
     (type-union
       )
     )
    (else (error 'complement (format "invalid lattice ~s" a)))))

;; Define implicit state monad threading for convenience?
(define (meet a b)
  )

(define (join a b)
  )
