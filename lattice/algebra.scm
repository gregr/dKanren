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

(defrecord var var? var-index)
(define (var=? t1 t2)
  (and (var? t1) (var? t2) (eqv? (var-index t1) (var-index t2))))
(define (var<? v1 v2) (< (var-index v1) (var-index v2)))

;; = + * - /
;; E ::= (= A A)
;; A ::= number | var | (+ A A) | (* A A) | (- A A)

;; simplification and normalization
;;
;; add
;; `(+ ,(? number? a) ,(? number? b))
;; (+ a b)
;;
;; multiply
;; `(* ,(? number? a) ,(? number? b))
;; (* a b)
;;
;; add-negative
;; `(- ,a ,b)
;; `(+ ,a (* -1 ,b))
;;
;; identity-add
;; `(+ ,a 0)
;; a
;;
;; identity-multiply
;; `(* ,a 1)
;; a
;;
;; identity-divide
;; `(/ ,a 1)
;; a
;;
;; cancel-multiply
;; `(* ,a 0)
;; 0
;;
;; divide-by-zero
;; `(/ ,a 0)
;; '(or)
;;
;; cancel-divide
;; `(/ 0 ,(and (not 0) a))
;; 0
;;
;; divide-invert-number
;; `(/ ,a ,(and (not 0) (? number? b)))
;; `(* ,a (/ 1 ,b))
;;
;; add-double
;; `(+ ,a ,a)
;; `(* ,a 2)
;;
;; collect-multiple
;; `(+ (* ,a ,(? number? b)) ,a)
;; `(* ,a (+ ,b 1))
;;
;; collect-multiples
;; `(+ (* ,a ,(? number? b)) (* ,a ,(? number? c)))
;; `(* ,a (+ ,b ,c))
;;
;; collect-multiples-double (can eliminate this with better normalization)
;; `(+ ,(or `(* ,a ,b) `(* ,b ,a)) ,(or `(* ,a ,b) `(* ,b ,a)))
;; `(* (* ,a ,b) 2)
;;
;; collect-target (can conflict with distribute if not careful)
;; need to identify variable being solved for
;;
;; commute-right-number
;; `(,(or '= '+ '* op) ,(? number? a) ,(and (not (? number?)) b))
;; `(,op ,b ,a)
;;
;; associate-right-number
;; `(,(or '+ '* op) (,op ,a ,(? number? b)) ,c)
;; `(,op ,a (,op ,b ,c))
;;
;; associate-left-nonnumber
;; `(,(or '+ '* op) ,a (,op ,(and (not (? number?)) b) ,c))
;; `(,op (,op ,a ,b) ,c)
;;
;; commute-into-division
;; `(* ,(? number? a) (/ ,b ,c))
;; `(* ,b (/ ,a ,c))
;;
;; associate-into-division
;; `(/ (* ,a ,(? number? b)) ,c)
;; `(* ,a (/ ,b ,c))
;;
;; distribute
;; `(* (+ ,a ,b) ,c)
;; `(+ (* ,a ,c) (* ,b ,c))
;;
;; isolate-right-add-number
;; `(= (+ ,a ,(? number? b)) ,c)
;; `(= ,a (- ,c ,b))
;;
;; isolate-left-add-nonnumber
;; `(= ,a ,(and (not (? number?)) b))
;; `(= (- ,a ,b) 0)
;;
;; isolate-right-multiply-number
;; `(= (/ ,a ,(? number? b)) ,c)
;; `(= ,a (* ,c ,b))
;;
;; isolate-right-divide-number
;; `(= (* ,a ,(? number? b)) ,c)
;; `(= ,a (/ ,c ,b))
;;
;; inversion laws on nonnumbers require that we case analyze =/= 0
;; `(or ,result (and (= ,inverted 0) ,original))


;; some laws
;;         (log U V) = W <-> V = U^W
;;             U - V = W <-> U = W + V
;;               U^2 = V <-> U = +-(sqrt V)
;;    (U + V) * ( U - V) <-> U^2 - V^2
;;             U*V + U*W <-> U * (V + W)
;; (log W U) + (log W V) <-> (log W (U * V))
;;               (W^U)^V <-> W^(U*V)

;; assumptions:
;; 1) term being rewritten is least dominating in x
;; 2) U and V contain x
;; 3) no other parts contain x
;;
;; attraction methods (move occurrences of x closer together)
;;
;;             W*U + W*V --> W * (U + V)
;; (log W U) + (log W V) --> (log W (U * V))
;;               (W^U)^V --> W^(U*V)
;;               U^(V*W) --> (U^V)^W
;;
;; collection methods (reduce number of occurrences of x)
;;
;; (U + V) * (U - V) --> U^2 - V^2
;;         U*W + U*Y --> U * (W + Y)
;;
;; isolation methods (reduce depth of occurrences of x)
;;
;; (log W U) = Y --> U = W^Y
;;     U - W = Y --> U = Y + w
;;       U^2 = W --> U = +-(sqrt W)
