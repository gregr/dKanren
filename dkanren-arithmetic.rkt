#lang racket/base
(provide
  arith
  build-num
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
                  (`(,_ (,_ ,_ . ,_) (1))
                    (match (adder d '(1) n)
                      ((and `(,_ ,_ . ,_) r) r)
                      (_ (gen-adder d n m))))
                  (`(,_ (,_ ,_ . ,_) ,_) (gen-adder d n m)))))
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
            )
     ,body))

(module+ test
  (mk-test "test 1"
    (run 1 (q) (dk-evalo (arith `(plus ',(build-num 2) ',(build-num 3))) q))
    '(((1 0 1))))

  (mk-test "test 2"
    (run 1 (q) (dk-evalo (arith `(* ',(build-num 2) ',(build-num 3))) q))
    '(((0 1 1))))

  (mk-test "test 3"
    (run* (n m) (dk-evalo (arith `(* ',n ',m)) (build-num 6)))
    '(((1) (0 1 1)) ((0 1 1) (1)) ((0 1) (1 1)) ((1 1) (0 1))))
  )
