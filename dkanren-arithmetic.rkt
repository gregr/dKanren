#lang racket/base
(provide
  arith
  build-num
  pluso
  *o
  )

(require
  "dkanren.rkt"
  racket/set
  )

(module+ test
  (require
    rackunit
    )

  (define-syntax mk-test-cont
    (syntax-rules ()
      ((_ test-name exact? query expected)
       (let* ((result-set (list->set query))
              (expected-set (list->set expected))
              (overlap (set-intersect result-set expected-set)))
         (if exact?
           (begin
             (when (not (equal? result-set expected-set))
               (displayln (format "failed test: ~a" test-name)))
             ;(check-equal? (set-subtract expected-set result-set) (set))
             ;(check-equal? (set-subtract result-set expected-set) (set))
             (check-equal? result-set expected-set))
           (check-equal? overlap expected-set))))))
  (define-syntax mk-test
    (syntax-rules ()
      ((_ test-name query expected)
        (mk-test-cont test-name #t query expected))))
  (define-syntax mk-test-subsumed
    (syntax-rules ()
      ((_ test-name query expected)
        (mk-test-cont test-name #f query expected))))
  (define-syntax mk-test-time
    (syntax-rules ()
      ((_ test-name query expected)
        (begin
          (displayln test-name)
          (time (mk-test-cont test-name #t query expected))))))

  (require racket/pretty)
  (define-syntax test
    (syntax-rules ()
     ((_ name expr expected)
      (let ((actual expr))
        (when (not (equal? actual expected))
          (display name)
          (newline)
          (pretty-print actual)
          (newline))
        (check-equal? actual expected)))))
  )

(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(define (arith body)
  `(letrec ((append (lambda (xs ys)
                      (if (null? xs)
                        ys
                        (cons (car xs) (append (cdr xs) ys)))))
            (pos? (lambda (n) (pair? n)))
            (>1? (lambda (n) (match n
                               (`(,a ,ad . ,dd) #t)
                               (_ #f))))
            (full-adder
              (lambda (b x y)
                (match `(,b ,x ,y)
                  ('(0 0 0) '(0 0))
                  ('(1 0 0) '(1 0))
                  ('(0 1 0) '(1 0))
                  ('(1 1 0) '(0 1))
                  ('(0 0 1) '(1 0))
                  ('(1 0 1) '(0 1))
                  ('(0 1 1) '(0 1))
                  ('(1 1 1) '(1 1)))))
            (adder
              (lambda (d n m)
                (match `(,d ,n ,m)
                  (`(0  ,_  ()       ) n)
                  (`(0  ()  (,_ . ,_)) m)
                  (`(1  ,_  ()       ) (adder 0 n '(1)))
                  (`(1  ()  (,_ . ,_)) (adder 0 '(1) m))
                  (`(,_ (1) (1)      ) (full-adder d 1 1))
                  (`(,_ (1) ,_       ) (gen-adder d n m))
                  ;; TODO: ideally, this could be written as two separate
                  ;; patterns without sacrificing performance.  See the
                  ;; commented clauses.
                  (`(,_ (,_ ,_ . ,_) ,_)
                    (match m
                      ('(1)
                       (match (adder d '(1) n)
                         ((and `(,_ ,_ . ,_) r) r)))
                      (`(,_ ,_ . ,_) (gen-adder d n m))))
                  ;; TODO: ideally, these two clauses, which share a common
                  ;; pattern prefix, would allow the prefix to be learned when
                  ;; these were the only two clauses remaining.
                  ;(`(,_ (,_ ,_ . ,_) (1))
                    ;(match (adder d '(1) n)
                      ;((and `(,_ ,_ . ,_) r) r)))
                  ;(`(,_ (,_ ,_ . ,_) (,_ ,_ . ,_)) (gen-adder d n m))
                  )))
            (gen-adder
              (lambda (d n m)
                (match `(,n ,m)
                  (`((,a . ,x) (,b . ,(and `(,_ . ,_) y)))
                    (match (full-adder d a b)
                      (`(,c ,e)
                        (match (adder e x y)
                          ((and `(,_ . ,_) z) `(,c . ,z)))))))))
            (plus (lambda (n m) (adder 0 n m)))
            (minus (lambda (n m) (fresh (k)
                                   (match `(,(plus m k) ,n)
                                     (`(,e ,e) k)
                                     (_ #f)))))

            (* (lambda (n m)
                 (match `(,n ,m)
                   (`(()       ,_      ) '())
                   (`(,_       ()      ) '())
                   (`((1)      ,_      ) m)
                   (`(,_       (1)     ) n)
                   (`((0 . ,x) ,_      ) `(0 . ,(* x m)))
                   (`((1 . ,x) (0 . ,y)) (* m n))
                   (`((1 . ,x) (1 . ,y)) (odd-* x n m)))))
            (odd-* (lambda (x n m)
                     (let ((q (* x m)))
                       (let ((p (plus `(0 . ,q) m)))
                         (and (bound-*? q p n m) p)))))
            (bound-*? (lambda (q p n m)
                        (match `(,q ,p)
                          (`(()         (,_ . ,_)) #t)
                          (`((,a0 . ,x) (,a1 . ,y))
                            (match `(,n ,m)
                              (`(()         (,a2 . ,z)) (bound-*? x y z '()))
                              (`((,a3 . ,z) ,_) (bound-*? x y z m)))))))

            ;(=l (lambda (n m)
                  ;(match `(,n ,m)
                    ;(`(() ()) #t)
                    ;(`((1) (1)) #t)
                    ;(`((,a . ,(and `(,_ . ,_) x)) (,b . ,(and `(,_ . ,_) y)))
                      ;(=l x y))
                    ;(_ #f))))
            ;(<l (lambda (n m)
                  ;(match `(,n ,m)
                    ;(`(() `(,_ . ,_)) #t)
                    ;(`((1) `(,_ ,_ . ,_)) #t)
                    ;(`((,a . ,(and `(,_ . ,_) x)) (,b . ,(and `(,_ . ,_) y)))
                      ;(<l x y))
                    ;(_ #f))))
            ;(<=l (lambda (n m) (or (=l n m) (<l n m))))
            ;(< (lambda (n m)
                 ;(or (<l n m)
                     ;(and (=l n m)
                          ;(match (minus m n)
                            ;(`(,_ . ,_) #t)
                            ;(_ #f))))))
            ;(<= (lambda (n m) (or (equal? n m) (< n m))))

            ;(split (lambda (n r)
                     ;(match `(,n ,r)
                       ;(`(()           ,_) '(() ()))
                       ;(`((0 ,b . ,n^) ()) `(() (,b . ,n^)))
                       ;(`((1 . ,n^)    ()) `((1) ,n^))
                       ;(`((0 ,b . ,n^) (,a . ,r^))
                         ;(match (split `(,b . ,n^) r^)
                           ;((and `(() ,h) result) result)
                           ;(_ #f)))
                       ;(`((1 . ,n^) (,a . ,r^))
                         ;(match (split n^ r^)
                           ;((and `(() ,h)) `((1) ,h))
                           ;(_ #f)))
                       ;(`((,b . ,n^) (,a . ,r^))
                         ;(match (split n^ r^)
                           ;(`(,(and `(,_ . ,_) l^) ,h) `((,b . ,l^) ,h))
                           ;(_ #f)))
                       ;(_ #f))))

            ;(/ (lambda (n m)
                 ;(if (< n m) `(() ,n)
                   ;(let ((r (minus n m)))
                     ;(if (and r (=l n m) (< r m)) `((1) ,r)
                       ;(fresh (q r qlmr rr rh)
                         ;(and (<l m n) (< r m) (pos? q)
                              ;(match (split n r)
                                ;(`(,nl ,nh)
                                  ;(match (split q r)
                                    ;(`(,ql ,qh)
                                      ;(match `(,nh ,qh)
                                        ;('(() ())
                                         ;(and (equal? (minus nl r) (* ql m))))
                                        ;(`((,_ . ,_) ,_)
                                          ;(match (split (minus (plus (* ql m) r) nl) r)
                                            ;(`(() ,rh)
                                              ;(and (equal? (/ nh m) `(,qh ,rh))))
                                            ;(_ #f)))
                                        ;(_ #f)))
                                    ;(_ #f)))
                                ;(_ #f))
                              ;`(,q ,r))))))))

            ;(exp2
              ;(lambda (n b)
                ;(let ((k (lambda ()
                           ;(fresh (q)
                             ;(let ((b2 (append b `(1 . ,b))))
                               ;(and (match q
                                      ;(`(0 . ,(and `(,_ . ,_) q1))
                                        ;(and (<l b n) (equal? (exp2 n b2) q1)))
                                      ;(`(1 . ,(and `(,_ . ,_) q1))
                                        ;(match (split n b)
                                          ;(`(,s ,(and `(,_ . ,_) nh))
                                            ;(equal? (exp2 nh b2) q1))
                                          ;(_ #f)))
                                      ;(_ #f))
                                    ;q))))))
                 ;(match n
                  ;('(1) '())
                  ;(`(,_ ,_ . ,_) (match (split n b)
                                   ;(`(,s (1)) '(1))
                                   ;(_ (k))))
                  ;(_ (k))))))

            ;(repeated-mul
              ;(lambda (n q)
                ;(match `(,n ,q)
                  ;(`((,_ . ,_) ()) '(1))
                  ;(`(,_ (1)) n)
                  ;(`(,_ (,_ ,_ . ,_))
                    ;(* (repeated-mul n (minus q '(1))) n)))))

            ;(log (lambda (n b)
                   ;(fresh (q r)
                     ;(and (match `(,n ,b ,q ,r)
                            ;(`((1) (,_ . ,_) () ()) #t)
                            ;(`(,_ ,_ () ,_)
                              ;(and (< n b) (equal? (plus r '(1)) n)))
                            ;(`(,_ (,_ ,_ . ,_) (1) ,_)
                              ;(and (=l n b) (equal? (plus r b) n)))
                            ;(`(,_ (1) (,_ . ,_) ,_) (equal? (plus r '(1)) n))
                            ;(`(,e () (,_ . ,_) ,e) #t)
                            ;(`((,a ,ad . ,(and `(,_ . ,_) dd)) (0 1) ,_ ,_)
                              ;(and (equal? (exp2 n '()) q)
                                   ;(match (split n dd)
                                     ;(`(,sr ,s) (equal? sr r))
                                     ;(_ #f))))
                            ;(`(,_ ,(or '(1 1) `(,_ ,_ ,_ . ,_)) ,_ ,_)
                              ;(and (let ((bw1 (exp2 b '()))
                                         ;(nw1 (exp2 n '())))
                                     ;(let ((bw (plus bw1 '(1)))
                                           ;(nw (plus nw1 '(1))))
                                       ;(match `(,(/ nw bw) ,(/ nw bw1))
                                         ;(`((,ql1 ,_) (,qh ,_))
                                           ;(let ((ql (minus ql1 '(1))))
                                             ;(let ((qd (minus q ql))
                                                   ;(qdh (minus qh ql)))
                                               ;(let ((bq (* (repeated-mul b ql)
                                                            ;(repeated-mul b qd))))
                                                 ;(and (<l b n) (<l q n)
                                                      ;(< nw1 (* bw (plus q '(1))))
                                                      ;(<= ql q) (<= qd qdh)
                                                      ;(equal? (plus bq r) n)
                                                      ;(< n (* b bq)))))))
                                         ;(_ #f))))))
                            ;(_ #f))
                          ;`(,q ,r)))))

            ;(exp (lambda (b q)
                   ;(fresh (n)
                     ;(match `(,q ,(log n b))
                       ;(`(,q (,q ())) #t)
                       ;(_ #f)))))
            )
     ,body))

(define (pluso a b c) (dk-evalo (arith `(plus ',a ',b)) c))
(define (*o a b c) (dk-evalo (arith `(* ',a ',b)) c))

(module+ test
  (mk-test "test 1"
    (run* (q) (pluso (build-num 2) (build-num 3) q) )
    '(((1 0 1))))

  (mk-test "test 2"
    (run* (q) (*o (build-num 2) (build-num 3) q))
    '(((0 1 1))))

  (mk-test "test 3"
    (run* (n m) (*o n m (build-num 6)))
    '(((1) (0 1 1)) ((0 1 1) (1)) ((0 1) (1 1)) ((1 1) (0 1))))

  (mk-test-subsumed "sums"
    (run 12 (x y z) (pluso x y z))
    '((_.0 () _.0)
      (() (_.0 . _.1) (_.0 . _.1))
      ((1) (1) (0 1))
      ((1) (0 _.0 . _.1) (1 _.0 . _.1))
      ((1) (1 1) (0 0 1))
      ((0 1) (0 1) (0 0 1))))

  ;(mk-test "factors"
    ;(run* (q)
      ;(fresh (x y)
        ;(*o x y (build-num 24))
        ;(== `(,x ,y ,(build-num 24)) q)))
    ;'(((((1) (0 0 0 1 1) (0 0 0 1 1))))
      ;((((0 0 0 1 1) (1) (0 0 0 1 1))))
      ;((((0 1) (0 0 1 1) (0 0 0 1 1))))
      ;((((0 0 1) (0 1 1) (0 0 0 1 1))))
      ;((((0 0 0 1) (1 1) (0 0 0 1 1))))
      ;((((1 1) (0 0 0 1) (0 0 0 1 1))))
      ;((((0 1 1) (0 0 1) (0 0 0 1 1))))
      ;((((0 0 1 1) (0 1) (0 0 0 1 1))))))

  ;(mk-test-time "logo 3 answers"
    ;(run 3 (b q r)
      ;(logo '(0 0 1 0 0 0 1) b q r)
      ;(>1o q))
    ;'(((() (_.0 _.1 . _.2) (0 0 1 0 0 0 1)))
      ;(((1) (_.0 _.1 . _.2) (1 1 0 0 0 0 1)))
      ;(((0 1) (0 1 1) (0 0 1)))))

  ;(mk-test-time "logo 9 answers"
    ;(run 9 (b q r)
      ;(logo '(0 0 1 0 0 0 1) b q r)
      ;(>1o q))
    ;'(((() (_.0 _.1 . _.2) (0 0 1 0 0 0 1)))
      ;(((1) (_.0 _.1 . _.2) (1 1 0 0 0 0 1)))
      ;(((0 1) (0 1 1) (0 0 1)))
      ;(((1 1) (1 1) (1 0 0 1 0 1)))
      ;(((0 0 1) (1 1) (0 0 1)))
      ;(((0 0 0 1) (0 1) (0 0 1)))
      ;(((1 0 1) (0 1) (1 1 0 1 0 1)))
      ;(((0 1 1) (0 1) (0 0 0 0 0 1)))
      ;(((1 1 1) (0 1) (1 1 0 0 1)))))
  )
