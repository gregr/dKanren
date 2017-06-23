(define-syntax let*/and
  (syntax-rules ()
    ((_ () rest ...) (and rest ...))
    ((_ ((name expr) ne* ...) rest ...)
     (let ((name expr))
       (and name (let*/and (ne* ...) rest ...))))))

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

(define store-empty '())
(define (store-ref store key . default)
  (let ((binding (assoc key store)))
    (if binding
      (cdr binding)
      (if (null? default)
        (error 'store-ref (format "missing key ~s in ~s" key store))
        (car default)))))
(define (store-set store key value) `((,key . ,value) . ,store))

(defrecord var var? var-index)
(define var/fresh
  (let ((index -1))
    (lambda ()
      (set! index (+ 1 index))
      (var index))))
(define var=? eq?)
(define (var<? v1 v2) (< (var-index v1) (var-index v2)))
(define var-initial (var/fresh))

(define (vattrs-get vs vr) (store-ref vs vr vr))
(define (vattrs-set vs vr value) (store-set vs vr value))
(define (walk-vs vs tm)
  (if (var? tm)
    (let ((va (vattrs-get vs tm)))
      (if (var=? tm va)
        tm
        (walk-vs vs va)))
    tm))

(defrecord state state? (state-vs set-state-vs))
(define state-empty (state store-empty))
(define (state-var-get st vr) (vattrs-get (state-vs st) vr))
(define (state-var-set st vr value)
  (set-state-vs st (vattrs-set (state-vs st) vr value)))
(define (state-var-== st vr value)
  (let*/and ((st (not-occurs? st vr value)))
    (state-var-set st vr value)))
(define (state-var-==-var st v1 v2)
  (if (var<? v1 v2)  ;; Pointing new to old may yield flatter substitutions.
    (state-var-set st v2 v1)
    (state-var-set st v1 v2)))

(define (walk st tm) (walk-vs (state-vs st) tm))

(define (not-occurs? st vr tm)
  (if (pair? tm)
    (let*/and ((st (not-occurs? st vr (walk st (car tm)))))
      (not-occurs? st vr (walk st (cdr tm))))
    (and (not (var=? vr tm)) st)))

(define (unify st t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond
      ((eqv? t1 t2) st)
      ((var? t1)
       (if (var? t2)
         (state-var-==-var st t1 t2)
         (state-var-== st t1 t2)))
      ((var? t2) (state-var-== st t2 t1))
      ((and (pair? t1) (pair? t2))
       (let*/and ((st (unify st (car t1) (car t2))))
         (unify st (cdr t1) (cdr t2))))
      (else #f))))

(defrecord conj conj? conj-c1 conj-c2)
(defrecord disj disj? disj-c1 disj-c2)
(defrecord zzz zzz? zzz-metadata zzz-wake)
(defrecord pause pause? pause-state pause-goal)
(defrecord == ==? ==-t1 ==-t2)

(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) body ...)
     (define (name param ...)
       (zzz `(name ,param ...) (lambda () body ...))))))

(define (bind ss goal)
  (cond
    ((not ss) #f)
    ((state? ss) (start ss goal))
    ((pair? ss) (mplus (start (car ss) goal) (conj (cdr ss) goal)))
    (else (conj ss goal))))
(define (mplus s1 s2)
  (cond
    ((not s1) s2)
    ((state? s1) (cons s1 s2))
    ((pair? s1) (cons (car s1) (disj s2 (cdr s1))))
    (else (disj s2 s1))))

(define (start st goal)
  (cond
    ((conj? goal) (bind (start st (conj-c1 goal)) (conj-c2 goal)))
    ((disj? goal) (disj (pause st (disj-c1 goal)) (pause st (disj-c2 goal))))
    ((zzz? goal) (start st ((zzz-wake goal))))
    ((==? goal) (unify st (==-t1 goal) (==-t2 goal)))))

(define (continue ss)
  (cond
    ((conj? ss) (bind (continue (conj-c1 ss)) (conj-c2 ss)))
    ((disj? ss) (mplus (continue (disj-c1 ss)) (disj-c2 ss)))
    ((pause? ss) (start (pause-state ss) (pause-goal ss)))))

(define (stream-take n ss)
  (cond
    ((and n (= 0 n)) '())
    ((not ss) '())
    ((state? ss) (list ss))
    ((pair? ss) (cons (car ss) (stream-take (and n (- n 1))
                                            (continue (cdr ss)))))
    (else (stream-take n (continue ss)))))

(define (goal-pretty goal)
  (cond
    ((conj? goal) `(conj ,(goal-pretty (conj-c1 goal)) ,(goal-pretty (conj-c2 goal))))
    ((disj? goal) `(disj ,(goal-pretty (disj-c1 goal)) ,(goal-pretty (disj-c2 goal))))
    ((zzz? goal) (zzz-metadata goal))
    ((==? goal) `(== ,(==-t1 goal) ,(==-t2 goal)))))
(define (stream-pretty ss)
  (define (pretty ss)
    (cond
      ((conj? ss)
       `(conj ,(pretty (conj-c1 ss)) ,(reify state-empty (goal-pretty (conj-c2 ss)))))
      ((disj? ss) `(disj ,(pretty (disj-c1 ss)) ,(pretty (disj-c2 ss))))
      ((pause? ss) (reify (pause-state ss) `(pause ,(goal-pretty (pause-goal ss)))))
      (else ss)))
  (let loop ((ss ss) (states '()))
    (cond
      ((state? ss) (loop #f (cons ss states)))
      ((pair? ss) (loop (cdr ss) (cons (car ss) states)))
      (else (list (map reify-initial (reverse states)) (pretty ss))))))

(define (step n ss)
  (cond
    ((= 0 n) ss)
    ((not ss) #f)
    ((pair? ss) (cons (car ss) (step n (cdr ss))))
    (else (step (- n 1) (continue ss)))))

;; TODO: steer, a continue that prompts for choices.

(define succeed (== #t #t))
(define fail (== #f #t))

(define-syntax conj*
  (syntax-rules ()
    ((_) succeed)
    ((_ g) g)
    ((_ gs ... g-final) (conj (conj* gs ...) g-final))))
(define-syntax disj*
  (syntax-rules ()
    ((_) fail)
    ((_ g) g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (vr ...) g0 gs ...) (let ((vr (var/fresh)) ...) (conj* g0 gs ...)))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 gs ...)) (conj* g0 gs ...))
    ((_ c0 cs ...) (disj (conde c0) (conde cs ...)))))

(define (run-goal n st goal) (stream-take n (start st goal)))

(define (reify st tm)
  (let loop ((rvs store-empty) (index 0) (tm tm) (k (lambda (rvs i tm) tm)))
    (let ((tm (walk st tm)))
      (cond
        ((var? tm)
         (let* ((idx (store-ref rvs tm index))
                (n (string->symbol (string-append "_." (number->string idx)))))
           (if (= index idx)
             (k (store-set rvs tm index) (+ 1 index) n)
             (k rvs index n))))
        ((pair? tm) (loop rvs index (car tm)
                          (lambda (r i a)
                            (loop r i (cdr tm)
                                  (lambda (r i d) (k r i `(,a . ,d)))))))
        (else (k rvs index tm))))))
(define (reify-initial st) (reify st var-initial))

(define-syntax query
  (syntax-rules ()
    ((_ (vr ...) g0 gs ...)
     (let ((goal (fresh (vr ...) (== (list vr ...) var-initial) g0 gs ...)))
       (start state-empty goal)))))
(define-syntax run
  (syntax-rules ()
    ((_ n body ...) (map reify-initial (stream-take n (query body ...))))))
(define-syntax run*
  (syntax-rules ()
    ((_ body ...) (run #f body ...))))


(define-syntax test
  (syntax-rules ()
    ((_ name expr expected-expr)
     (begin
       (printf "Testing ~s: " name)
       (let* ((expected expected-expr) (actual expr))
         (if (equal? expected actual)
           (printf "Succeeded.\n")
           (printf "\nFailed: ~a\nExpected: ~a\nActual: ~a\n"
                   'expr expected actual)))))))

(define-relation (appendo l s ls)
  (conde
    ((== '() l) (== s ls))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) ls)
       (appendo d s res)))))

(test 'appendo-1
  (run* (q) (appendo '(a b c) '(d e) q))
  '(((a b c d e))))
(test 'appendo-2
  (run* (q) (appendo '(a b c) q '(a b c d e)))
  '(((d e))))
(test 'appendo-3
  (run* (q) (appendo q '(d e) '(a b c d e)))
  '(((a b c))))
(test 'appendo-4
  (run* (p q) (appendo p q '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))


(define (evalo expr value) (eval-expo expr '() value))
(define-relation (eval-expo expr env value)
  (conde
    ;; Placing lambdas first seems to reduce quoted closures.
    ((fresh (body)
       (== `(lambda ,body) expr)
       (== `(closure ,body ,env) value)))
    ((fresh (datum)
       (== `(quote ,datum) expr)
       (== datum value)))
    ((fresh (a*)
       (== `(list . ,a*) expr)
       (eval-listo a* env value)))
    ((fresh (index)
       (== `(var ,index) expr)
       (lookupo index env value)))
    ((fresh (rator rand arg env^ body)
       (== `(app ,rator ,rand) expr)
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `(,arg . ,env^) value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (c va vd)
       (== `(car ,c) expr)
       (== va value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (c va vd)
       (== `(cdr ,c) expr)
       (== vd value)
       (eval-expo c env `(,va . ,vd))))
    ))

;; Avoiding quoted closures in this way is too slow.
;(define-relation (quotedo datum)
  ;(conde
    ;((fresh (a d)
       ;(== `(,a . ,d) datum)
       ;(quotedo a)
       ;(quotedo d)))
    ;((== 'quote datum))
    ;((== 'list datum))
    ;((== 'lambda datum))
    ;((== 'app datum))
    ;((== 'var datum))
    ;((== '() datum))
    ;((== 's datum))))

(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i*)
         (== `(s . ,i*) index)
         (lookupo i* e* value))))))

(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))

;; First order version that avoids quoted closures.
;(define (evalo expr value) (eval-expo expr '() value))
;(define-relation (eval-expo expr env value)
  ;(conde
    ;((fresh (datum)
       ;(== `(quote ,datum) expr)
       ;(== datum value)))
    ;((fresh (a*)
       ;(== `(list . ,a*) expr)
       ;(eval-listo a* env value)))
    ;((fresh (index)
       ;(== `(var ,index) expr)
       ;(lookupo index env value)))
    ;((fresh (rator rand arg body)
       ;(== `(app ,rator ,rand) expr)
       ;(== `(lambda ,body) rator)
       ;(eval-expo rand env arg)
       ;(eval-expo body `(,arg . ,env) value)))))

(time (test 'evalo-1
  (run 1 (q) (evalo q q))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
(time (test 'evalo-step-1
  (car (stream-pretty (step 11811 (query (q) (evalo q q)))))
  '()))
(time (test 'evalo-step-2
  (car (stream-pretty (step 11812 (query (q) (evalo q q)))))
  '(((app
       (lambda (list 'app (var ()) (list 'quote (var ()))))
       '(lambda (list 'app (var ()) (list 'quote (var ())))))))))
