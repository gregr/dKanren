(load "list.scm")
(load "ordered-set.scm")

;; A real-set stores sorted, open intervals of the form (lb . ub) where lb
;; is the lower bound and ub is the upper bound.  The bounds may be #f,
;; representing negative and positive infinity, depending on which side they
;; are placed.  Individual numbers are stored sorted alongside the intervals,
;; and these represent included points.  e.g., the set (2 (4 . 8) 8 (10 . f))
;; represents a union of the point or the closed interval [2 2] with the
;; half-closed interval (4 8] and the open interval (10 +infinity), i.e., all
;; numbers x such that either x = 2 OR 4 < x <= 8 OR 10 < x).
(define interval-full '(#f . #f))
(define (interval-complement x)
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

(define real-set-empty '())
(define real-set-full `(,interval-full))

(define (real-set-join a b)
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

(define (real-set-meet-interval x ns)
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
           (cons i (real-set-meet-interval x (cdr ns))))
         (lambda ()  ;; gt-overlap
           (cons (interval-overlap-meet i x)
                 (real-set-meet-interval x (cdr ns))))
         (lambda ()  ;; gt
           (real-set-meet-interval x (cdr ns))))))))

(define (real-set-meet a b)
  (list-foldr
    (lambda (x ns)
      (real-set-join (real-set-meet-interval x b) ns))
    '()
    a))

(define (real-set-complement ns)
  (list-foldr real-set-meet real-set-full (map interval-complement ns)))


(define (real-set< n) `((#f . ,n)))
(define (real-set<= n) `((#f . ,n) ,n))
(define (real-set>= n) `(,n (,n . #f)))
(define (real-set> n) `((,n . #f)))

;; Only use these for defining sets in terms of points.
(define (real-set-with ns) (ordered-set ns))
(define (real-set-without ns) (real-set-complement (real-set-with ns)))

(define (real-set-widen rs)
  (define fst (car rs))
  (define lst (list-last rs))
  (define lb (if (number? fst) fst (car fst)))
  (define ub (if (number? lst) lst (cdr lst)))
  (define suffix (cons (cons lb ub) (if (number? lst) (list ub) '())))
  (if (number? fst) (cons lb suffix) suffix))

(define (real-set-cross combine cross as bs)
  (merge-sort
    real-set-join
    (list-foldr
      (lambda (a rss)
        (list-foldr (lambda (b rss) (combine (cross a b) rss)) rss bs))
      '() as)))

(define (interval+ a b)
  (define (ip+ i p)
    (cons (and (car i) (+ p (car i))) (and (cdr i) (+ p (cdr i)))))
  (if (number? a)
    (if (number? b) (+ a b) (ip+ b a))
    (if (number? b) (ip+ a b)
      (cons (and (car a) (car b) (+ (car a) (car b)))
            (and (cdr a) (cdr b) (+ (cdr a) (cdr b)))))))
(define (interval- a b) (interval+ a (if (number? b) (- b) (interval* b -1))))

(define (interval* a b)
  (define (ip* i p)
    (define ia (and (car i) (* p (car i))))
    (define id (and (cdr i) (* p (cdr i))))
    (cond ((< p 0) (cons id ia)) ((> p 0) (cons ia id)) (else 0)))
  (if (number? a)
    (if (number? b) (* a b) (ip* b a))
    (if (number? b) (ip* a b)
      (let* ((al (car a))
             (au (cdr a))
             (bl (car b))
             (bu (cdr b))
             (apos? (and al (<= 0 al)))
             (aneg? (and au (<= au 0)))
             (bpos? (and bl (<= 0 bl)))
             (bneg? (and bu (<= bu 0))))
        (if apos?
          (if bpos? (cons (* al bl) (and au bu (* au bu)))
            (if bneg? (cons (and au bl (* au bl)) (* al bu))
              (cons (and au bl (* au bl)) (and au bu (* au bu)))))
          (if aneg?
            (if bpos? (cons (and al bu (* al bu)) (* au bl))
              (if bneg? (cons (* au bu) (and al bl (* al bl)))
                (cons (and al bu (* al bu)) (and al bl (* al bl)))))
            (if bpos?
              (cons (and bu al (* bu al)) (and bu au (* bu au)))
              (if bneg?
                (cons (and bl au (* bl au)) (and bl al (* bl al)))
                (let ((nn (and al bl (* al bl)))
                      (pp (and au bu (* au bu)))
                      (np (and al bu (* al bu)))
                      (pn (and au bl (* au bl))))
                  (cons (and np pn (min np pn)) (and nn pp (max nn pp))))))))))))

(define (interval-invert ival)
  (define (pinv p) (if p (/ 1 p) 0))
  (cond
    ((eqv? 0 ival) '())
    ((number? ival) (list (/ 1 ival)))
    (else
      (let ((lb (car ival)) (ub (cdr ival)))
        (cond
          ((or (and lb (< 0 lb)) (and ub (< ub 0)))
           (list (cons (pinv ub) (pinv lb))))
          ((eqv? 0 lb) (list (cons (pinv ub) #f)))
          ((eqv? 0 ub) (list (cons #f (pinv lb))))
          (else (real-set-join (list (cons #f (pinv lb)))
                               (list (cons (pinv ub) #f)))))))))
(define (interval/ a b) (real-set* (list a) (interval-invert b)))

(define (real-set+ as bs) (real-set-cross cons interval+ as bs))
(define (real-set- as bs) (real-set-cross cons interval- as bs))
(define (real-set* as bs) (real-set-cross cons interval* as bs))
(define (real-set/ as bs) (real-set-cross append interval/ as bs))
