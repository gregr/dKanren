(define q-1p
  (query (defn)
    (fresh (body)
      (== `(lambda ,body) defn)
      (evalo `(list (app ,defn '(x)) (app ,defn '(y)))
             '((1 x) (1 y))))))


;; (stream-pretty q-1p)
;; Initial snapshot

'(()
  (disj
   (pause
    (state ((lambda _.0)))
    (conj
     (== (lambda _.0) (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
     (== (closure _.0 ()) ((1 x) (1 y)))))
   (pause
    (state ((lambda _.0)))
    (disj
     (conj
      (== '_.0 (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
      (== _.0 ((1 x) (1 y))))
     (disj
      (conj
       (== (list . _.2) (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
       (eval-listo _.2 () ((1 x) (1 y))))
      (disj
       (conj
        (== (var _.3) (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
        (lookupo _.3 () ((1 x) (1 y))))
       (disj
        (conj
         (conj
          (conj
           (==
            (app _.4 _.5)
            (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
           (eval-expo _.4 () (closure _.6 _.7)))
          (eval-expo _.5 () _.8))
         (eval-expo _.6 (_.8 . _.7) ((1 x) (1 y))))
        (disj
         (conj
          (conj
           (conj
            (==
             (cons _.9 _.10)
             (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
            (== (_.11 . _.12) ((1 x) (1 y))))
           (eval-expo _.9 () _.11))
          (eval-expo _.10 () _.12))
         (disj
          (conj
           (conj
            (==
             (car _.13)
             (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
            (== _.14 ((1 x) (1 y))))
           (eval-expo _.13 () (_.14 . _.15)))
          (conj
           (conj
            (==
             (cdr _.16)
             (list (app (lambda _.1) '(x)) (app (lambda _.1) '(y))))
            (== _.17 ((1 x) (1 y))))
           (eval-expo _.16 () (_.18 . _.17))))))))))))


;; (stream-pretty (step 50 q-1p))
;; Snapshot 50 steps in

'(()
  (disj
   (conj
    (pause
     (state ((lambda '(1 x))))
     (disj
      (conj (== '_.0 (app (lambda '(1 x)) '(y))) (== _.0 (1 y)))
      (disj
       (conj
        (== (list . _.1) (app (lambda '(1 x)) '(y)))
        (eval-listo _.1 () (1 y)))
       (disj
        (conj (== (var _.2) (app (lambda '(1 x)) '(y))) (lookupo _.2 () (1 y)))
        (disj
         (conj
          (conj
           (conj
            (== (app _.3 _.4) (app (lambda '(1 x)) '(y)))
            (eval-expo _.3 () (closure _.5 _.6)))
           (eval-expo _.4 () _.7))
          (eval-expo _.5 (_.7 . _.6) (1 y)))
         (disj
          (conj
           (conj
            (conj
             (== (cons _.8 _.9) (app (lambda '(1 x)) '(y)))
             (== (_.10 . _.11) (1 y)))
            (eval-expo _.8 () _.10))
           (eval-expo _.9 () _.11))
          (disj
           (conj
            (conj (== (car _.12) (app (lambda '(1 x)) '(y))) (== _.13 (1 y)))
            (eval-expo _.12 () (_.13 . _.14)))
           (conj
            (conj (== (cdr _.15) (app (lambda '(1 x)) '(y))) (== _.16 (1 y)))
            (eval-expo _.15 () (_.17 . _.16))))))))))
    (eval-listo _.0 () _.1))
   (conj
    (disj
     (conj
      (disj
       (pause
        (state ((lambda _.0)))
        (disj
         (conj (== (var _.0) '(x)) (lookupo _.0 () _.1))
         (disj
          (conj
           (conj
            (conj (== (app _.2 _.3) '(x)) (eval-expo _.2 () (closure _.4 _.5)))
            (eval-expo _.3 () _.6))
           (eval-expo _.4 (_.6 . _.5) _.1))
          (disj
           (conj
            (conj
             (conj (== (cons _.7 _.8) '(x)) (== (_.9 . _.10) _.1))
             (eval-expo _.7 () _.9))
            (eval-expo _.8 () _.10))
           (disj
            (conj
             (conj (== (car _.11) '(x)) (== _.12 _.1))
             (eval-expo _.11 () (_.12 . _.13)))
            (conj
             (conj (== (cdr _.14) '(x)) (== _.15 _.1))
             (eval-expo _.14 () (_.16 . _.15))))))))
       (conj
        (pause
         (state ((lambda _.0)))
         (disj
          (conj
           (conj
            (conj
             (== (app _.0 _.1) (lambda _.2))
             (eval-expo _.0 () (closure _.3 _.4)))
            (eval-expo _.1 () _.5))
           (eval-expo _.3 (_.5 . _.4) (closure _.6 _.7)))
          (disj
           (conj
            (conj
             (conj
              (== (cons _.8 _.9) (lambda _.2))
              (== (_.10 . _.11) (closure _.6 _.7)))
             (eval-expo _.8 () _.10))
            (eval-expo _.9 () _.11))
           (disj
            (conj
             (conj (== (car _.12) (lambda _.2)) (== _.13 (closure _.6 _.7)))
             (eval-expo _.12 () (_.13 . _.14)))
            (conj
             (conj (== (cdr _.15) (lambda _.2)) (== _.16 (closure _.6 _.7)))
             (eval-expo _.15 () (_.17 . _.16)))))))
        (eval-expo _.0 () _.1)))
      (eval-expo _.0 (_.1 . _.2) _.3))
     (disj
      (pause
       (state ((lambda _.0)))
       (disj
        (conj (== (var _.0) _.1) (lookupo _.0 ((x)) (1 x)))
        (disj
         (conj
          (conj
           (conj
            (== (app _.2 _.3) _.1)
            (eval-expo _.2 ((x)) (closure _.4 _.5)))
           (eval-expo _.3 ((x)) _.6))
          (eval-expo _.4 (_.6 . _.5) (1 x)))
         (disj
          (conj
           (conj
            (conj (== (cons _.7 _.8) _.1) (== (_.9 . _.10) (1 x)))
            (eval-expo _.7 ((x)) _.9))
           (eval-expo _.8 ((x)) _.10))
          (disj
           (conj
            (conj (== (car _.11) _.1) (== _.12 (1 x)))
            (eval-expo _.11 ((x)) (_.12 . _.13)))
           (conj
            (conj (== (cdr _.14) _.1) (== _.15 (1 x)))
            (eval-expo _.14 ((x)) (_.16 . _.15))))))))
      (disj
       (pause (state ((lambda (list . _.0)))) (conj (== () _.0) (== () (1 x))))
       (pause
        (state ((lambda (list . _.0))))
        (conj
         (conj
          (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (1 x)))
          (eval-expo _.0 ((x)) _.3))
         (eval-listo _.1 ((x)) _.4))))))
    (eval-listo _.0 () _.1))))


;; (stream-pretty (step 1607 q-1p))
;; Snapshot right before the first answer appears

'(()
  (disj
   (disj
    (disj
     (pause (state ((lambda (cons '1 (var ()))))) (conj (== () ()) (== () ())))
     (pause
      (state ((lambda (cons '1 (var ())))))
      (conj
       (conj
        (conj (== (_.0 . _.1) ()) (== (_.2 . _.3) ()))
        (eval-expo _.0 () _.2))
       (eval-listo _.1 () _.3))))
    (conj
     (disj
      (disj
       (disj
        (pause
         (state ((lambda (cons '1 (var ())))))
         (conj
          (conj
           (conj
            (== (app _.0 _.1) (var ()))
            (eval-expo _.0 ((y)) (closure _.2 _.3)))
           (eval-expo _.1 ((y)) _.4))
          (eval-expo _.2 (_.4 . _.3) (y))))
        (pause
         (state ((lambda (cons '1 (var ())))))
         (disj
          (conj
           (conj
            (conj (== (cons _.0 _.1) (var ())) (== (_.2 . _.3) (y)))
            (eval-expo _.0 ((y)) _.2))
           (eval-expo _.1 ((y)) _.3))
          (disj
           (conj
            (conj (== (car _.4) (var ())) (== _.5 (y)))
            (eval-expo _.4 ((y)) (_.5 . _.6)))
           (conj
            (conj (== (cdr _.7) (var ())) (== _.8 (y)))
            (eval-expo _.7 ((y)) (_.9 . _.8)))))))
       (pause
        (state ((lambda (cons '1 (var ())))))
        (conj (== (s . _.0) ()) (lookupo _.0 () (y)))))
      (conj
       (pause
        (state ((lambda (cons '1 (var ())))))
        (conj
         (conj (== (cdr _.0) '1) (== _.1 1))
         (eval-expo _.0 ((y)) (_.2 . _.1))))
       (eval-expo _.0 (_.1 . _.2) _.3)))
     (eval-listo _.0 () _.1)))
   (disj
    (disj
     (conj
      (disj
       (disj
        (disj
         (disj
          (disj
           (conj
            (disj
             (disj
              (pause
               (state ((lambda (list (cdr _.0) . _.1))))
               (disj
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (_.2 . 1)))
                (disj
                 (conj (== (var _.3) _.1) (lookupo _.3 ((x)) (_.2 . 1)))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.4 _.5) _.1)
                     (eval-expo _.4 ((x)) (closure _.6 _.7)))
                    (eval-expo _.5 ((x)) _.8))
                   (eval-expo _.6 (_.8 . _.7) (_.2 . 1)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (cons _.9 _.10) _.1)
                      (== (_.11 . _.12) (_.2 . 1)))
                     (eval-expo _.9 ((x)) _.11))
                    (eval-expo _.10 ((x)) _.12))
                   (disj
                    (conj
                     (conj (== (car _.13) _.1) (== _.14 (_.2 . 1)))
                     (eval-expo _.13 ((x)) (_.14 . _.15)))
                    (conj
                     (conj (== (cdr _.16) _.1) (== _.17 (_.2 . 1)))
                     (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
              (disj
               (pause
                (state ((lambda (list (car _.0) . _.1))))
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (1 . _.2))))
               (pause
                (state ((lambda (list (car _.0) . _.1))))
                (disj
                 (conj (== (var _.0) _.1) (lookupo _.0 ((x)) (1 . _.2)))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.3 _.4) _.1)
                     (eval-expo _.3 ((x)) (closure _.5 _.6)))
                    (eval-expo _.4 ((x)) _.7))
                   (eval-expo _.5 (_.7 . _.6) (1 . _.2)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (cons _.8 _.9) _.1)
                      (== (_.10 . _.11) (1 . _.2)))
                     (eval-expo _.8 ((x)) _.10))
                    (eval-expo _.9 ((x)) _.11))
                   (disj
                    (conj
                     (conj (== (car _.12) _.1) (== _.13 (1 . _.2)))
                     (eval-expo _.12 ((x)) (_.13 . _.14)))
                    (conj
                     (conj (== (cdr _.15) _.1) (== _.16 (1 . _.2)))
                     (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
             (disj
              (disj
               (pause
                (state ((lambda (list (app (lambda _.0) (lambda _.1)) . _.2))))
                (conj
                 (== (list . _.0) _.1)
                 (eval-listo _.0 ((closure _.2 ((x))) (x)) 1)))
               (pause
                (state ((lambda (list (app (lambda _.0) (lambda _.1)) . _.2))))
                (disj
                 (conj
                  (== (var _.0) _.1)
                  (lookupo _.0 ((closure _.2 ((x))) (x)) 1))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.3 _.4) _.1)
                     (eval-expo
                      _.3
                      ((closure _.2 ((x))) (x))
                      (closure _.5 _.6)))
                    (eval-expo _.4 ((closure _.2 ((x))) (x)) _.7))
                   (eval-expo _.5 (_.7 . _.6) 1))
                  (disj
                   (conj
                    (conj
                     (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) 1))
                     (eval-expo _.8 ((closure _.2 ((x))) (x)) _.10))
                    (eval-expo _.9 ((closure _.2 ((x))) (x)) _.11))
                   (disj
                    (conj
                     (conj (== (car _.12) _.1) (== _.13 1))
                     (eval-expo _.12 ((closure _.2 ((x))) (x)) (_.13 . _.14)))
                    (conj
                     (conj (== (cdr _.15) _.1) (== _.16 1))
                     (eval-expo
                      _.15
                      ((closure _.2 ((x))) (x))
                      (_.17 . _.16)))))))))
              (disj
               (disj
                (pause
                 (state ((lambda (list (app (lambda _.0) '_.1) . _.2))))
                 (conj (== (lambda _.0) _.1) (== (closure _.0 (_.2 (x))) 1)))
                (pause
                 (state ((lambda (list (app (lambda _.0) '_.1) . _.2))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 1))
                  (disj
                   (conj (== (list . _.2) _.1) (eval-listo _.2 (_.3 (x)) 1))
                   (disj
                    (conj (== (var _.4) _.1) (lookupo _.4 (_.3 (x)) 1))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.5 _.6) _.1)
                        (eval-expo _.5 (_.3 (x)) (closure _.7 _.8)))
                       (eval-expo _.6 (_.3 (x)) _.9))
                      (eval-expo _.7 (_.9 . _.8) 1))
                     (disj
                      (conj
                       (conj
                        (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) 1))
                        (eval-expo _.10 (_.3 (x)) _.12))
                       (eval-expo _.11 (_.3 (x)) _.13))
                      (disj
                       (conj
                        (conj (== (car _.14) _.1) (== _.15 1))
                        (eval-expo _.14 (_.3 (x)) (_.15 . _.16)))
                       (conj
                        (conj (== (cdr _.17) _.1) (== _.18 1))
                        (eval-expo _.17 (_.3 (x)) (_.19 . _.18)))))))))))
               (disj
                (conj
                 (disj
                  (pause
                   (state ((lambda (list (app (lambda _.0) _.1) . _.2))))
                   (disj
                    (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                    (disj
                     (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (app _.4 _.5) _.1)
                         (eval-expo _.4 ((x)) (closure _.6 _.7)))
                        (eval-expo _.5 ((x)) _.8))
                       (eval-expo _.6 (_.8 . _.7) _.2))
                      (disj
                       (conj
                        (conj
                         (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                         (eval-expo _.9 ((x)) _.11))
                        (eval-expo _.10 ((x)) _.12))
                       (disj
                        (conj
                         (conj (== (car _.13) _.1) (== _.14 _.2))
                         (eval-expo _.13 ((x)) (_.14 . _.15)))
                        (conj
                         (conj (== (cdr _.16) _.1) (== _.17 _.2))
                         (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                  (disj
                   (conj
                    (disj
                     (pause
                      (state ((lambda (list (app _.0 _.1) . _.2))))
                      (conj
                       (== (list . _.0) _.1)
                       (eval-listo _.0 ((x)) (closure _.2 _.3))))
                     (pause
                      (state ((lambda (list (app _.0 _.1) . _.2))))
                      (disj
                       (conj
                        (== (var _.0) _.1)
                        (lookupo _.0 ((x)) (closure _.2 _.3)))
                       (disj
                        (conj
                         (conj
                          (conj
                           (== (app _.4 _.5) _.1)
                           (eval-expo _.4 ((x)) (closure _.6 _.7)))
                          (eval-expo _.5 ((x)) _.8))
                         (eval-expo _.6 (_.8 . _.7) (closure _.2 _.3)))
                        (disj
                         (conj
                          (conj
                           (conj
                            (== (cons _.9 _.10) _.1)
                            (== (_.11 . _.12) (closure _.2 _.3)))
                           (eval-expo _.9 ((x)) _.11))
                          (eval-expo _.10 ((x)) _.12))
                         (disj
                          (conj
                           (conj
                            (== (car _.13) _.1)
                            (== _.14 (closure _.2 _.3)))
                           (eval-expo _.13 ((x)) (_.14 . _.15)))
                          (conj
                           (conj
                            (== (cdr _.16) _.1)
                            (== _.17 (closure _.2 _.3)))
                           (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                    (eval-expo _.0 (_.1 . _.2) _.3))
                   (pause
                    (state
                     ((lambda (list (app '(closure _.0 _.1) _.2) . _.3))))
                    (disj
                     (conj (== '_.0 _.1) (== _.0 _.2))
                     (disj
                      (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                      (disj
                       (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                       (disj
                        (conj
                         (conj
                          (conj
                           (== (app _.5 _.6) _.1)
                           (eval-expo _.5 ((x)) (closure _.7 _.8)))
                          (eval-expo _.6 ((x)) _.9))
                         (eval-expo _.7 (_.9 . _.8) _.2))
                        (disj
                         (conj
                          (conj
                           (conj
                            (== (cons _.10 _.11) _.1)
                            (== (_.12 . _.13) _.2))
                           (eval-expo _.10 ((x)) _.12))
                          (eval-expo _.11 ((x)) _.13))
                         (disj
                          (conj
                           (conj (== (car _.14) _.1) (== _.15 _.2))
                           (eval-expo _.14 ((x)) (_.15 . _.16)))
                          (conj
                           (conj (== (cdr _.17) _.1) (== _.18 _.2))
                           (eval-expo _.17 ((x)) (_.19 . _.18))))))))))))
                 (eval-expo _.0 (_.1 . _.2) _.3))
                (disj
                 (pause
                  (state
                   ((lambda (list
                             (app '(closure _.0 _.1) (lambda _.2))
                             .
                             _.3))))
                  (conj
                   (== (lambda _.0) _.1)
                   (== (closure _.0 ((closure _.2 ((x))) . _.3)) 1)))
                 (pause
                  (state
                   ((lambda (list
                             (app '(closure _.0 _.1) (lambda _.2))
                             .
                             _.3))))
                  (disj
                   (conj (== '_.0 _.1) (== _.0 1))
                   (disj
                    (conj
                     (== (list . _.2) _.1)
                     (eval-listo _.2 ((closure _.3 ((x))) . _.4) 1))
                    (disj
                     (conj
                      (== (var _.5) _.1)
                      (lookupo _.5 ((closure _.3 ((x))) . _.4) 1))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (app _.6 _.7) _.1)
                         (eval-expo
                          _.6
                          ((closure _.3 ((x))) . _.4)
                          (closure _.8 _.9)))
                        (eval-expo _.7 ((closure _.3 ((x))) . _.4) _.10))
                       (eval-expo _.8 (_.10 . _.9) 1))
                      (disj
                       (conj
                        (conj
                         (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) 1))
                         (eval-expo _.11 ((closure _.3 ((x))) . _.4) _.13))
                        (eval-expo _.12 ((closure _.3 ((x))) . _.4) _.14))
                       (disj
                        (conj
                         (conj (== (car _.15) _.1) (== _.16 1))
                         (eval-expo
                          _.15
                          ((closure _.3 ((x))) . _.4)
                          (_.16 . _.17)))
                        (conj
                         (conj (== (cdr _.18) _.1) (== _.19 1))
                         (eval-expo
                          _.18
                          ((closure _.3 ((x))) . _.4)
                          (_.20 . _.19)))))))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (conj
            (pause
             (state ((lambda (list (cdr '(_.0 . 1)) _.1 . _.2))))
             (disj
              (conj (== '_.0 _.1) (== _.0 x))
              (disj
               (conj (== (list . _.2) _.1) (eval-listo _.2 ((x)) x))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 ((x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 ((x)) (closure _.6 _.7)))
                   (eval-expo _.5 ((x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) x))
                    (eval-expo _.9 ((x)) _.11))
                   (eval-expo _.10 ((x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 x))
                    (eval-expo _.13 ((x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 x))
                    (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))
          (disj
           (pause
            (state ((lambda (list (car '(1 . _.0)) 'x . _.1))))
            (conj
             (conj
              (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) ()))
              (eval-expo _.0 ((x)) _.3))
             (eval-listo _.1 ((x)) _.4)))
           (conj
            (disj
             (pause
              (state ((lambda (list (car '(1 . _.0)) _.1 . _.2))))
              (disj
               (conj (== (var _.0) _.1) (lookupo _.0 ((x)) x))
               (disj
                (conj
                 (conj
                  (conj
                   (== (app _.2 _.3) _.1)
                   (eval-expo _.2 ((x)) (closure _.4 _.5)))
                  (eval-expo _.3 ((x)) _.6))
                 (eval-expo _.4 (_.6 . _.5) x))
                (disj
                 (conj
                  (conj
                   (conj (== (cons _.7 _.8) _.1) (== (_.9 . _.10) x))
                   (eval-expo _.7 ((x)) _.9))
                  (eval-expo _.8 ((x)) _.10))
                 (disj
                  (conj
                   (conj (== (car _.11) _.1) (== _.12 x))
                   (eval-expo _.11 ((x)) (_.12 . _.13)))
                  (conj
                   (conj (== (cdr _.14) _.1) (== _.15 x))
                   (eval-expo _.14 ((x)) (_.16 . _.15))))))))
             (disj
              (pause
               (state ((lambda (list (car '(1 . _.0)) (list . _.1) . _.2))))
               (conj (== () _.0) (== () x)))
              (pause
               (state ((lambda (list (car '(1 . _.0)) (list . _.1) . _.2))))
               (conj
                (conj
                 (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) x))
                 (eval-expo _.0 ((x)) _.3))
                (eval-listo _.1 ((x)) _.4)))))
            (eval-listo _.0 (_.1 . _.2) _.3))))
         (conj
          (disj
           (pause
            (state ((lambda (list (app (lambda '1) (lambda _.0)) _.1 . _.2))))
            (disj
             (conj
              (conj
               (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) x))
               (eval-expo _.0 ((x)) _.3))
              (eval-expo _.1 ((x)) _.4))
             (disj
              (conj
               (conj (== (car _.5) _.2) (== _.6 x))
               (eval-expo _.5 ((x)) (_.6 . _.7)))
              (conj
               (conj (== (cdr _.8) _.2) (== _.9 x))
               (eval-expo _.8 ((x)) (_.10 . _.9))))))
           (conj
            (conj
             (disj
              (pause
               (state
                ((lambda (list
                          (app (lambda '1) (lambda _.0))
                          (app _.1 _.2)
                          .
                          _.3))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((x))) (closure _.2 _.3))))
              (pause
               (state
                ((lambda (list
                          (app (lambda '1) (lambda _.0))
                          (app _.1 _.2)
                          .
                          _.3))))
               (disj
                (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3)))
                (disj
                 (conj
                  (== (list . _.4) _.1)
                  (eval-listo _.4 ((x)) (closure _.2 _.3)))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((x)) (closure _.2 _.3)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo _.6 ((x)) (closure _.8 _.9)))
                     (eval-expo _.7 ((x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) (closure _.2 _.3)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.11 _.12) _.1)
                       (== (_.13 . _.14) (closure _.2 _.3)))
                      (eval-expo _.11 ((x)) _.13))
                     (eval-expo _.12 ((x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 (closure _.2 _.3)))
                      (eval-expo _.15 ((x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 (closure _.2 _.3)))
                      (eval-expo _.18 ((x)) (_.20 . _.19)))))))))))
             (eval-expo _.0 (_.1 . _.2) _.3))
            (eval-expo _.0 (_.1 . _.2) _.3)))
          (eval-listo _.0 (_.1 . _.2) _.3)))
        (conj
         (disj
          (disj
           (disj
            (disj
             (disj
              (pause
               (state
                ((lambda (list
                          '1
                          (app '(closure _.0 _.1) (lambda _.2))
                          .
                          _.3))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) . _.3)) x)))
              (pause
               (state
                ((lambda (list
                          '1
                          (app '(closure _.0 _.1) (lambda _.2))
                          .
                          _.3))))
               (disj
                (conj (== '_.0 _.1) (== _.0 x))
                (disj
                 (conj
                  (== (list . _.2) _.1)
                  (eval-listo _.2 ((closure _.3 ((x))) . _.4) x))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.3 ((x))) . _.4) x))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.3 ((x))) . _.4)
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.3 ((x))) . _.4) _.10))
                    (eval-expo _.8 (_.10 . _.9) x))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) x))
                      (eval-expo _.11 ((closure _.3 ((x))) . _.4) _.13))
                     (eval-expo _.12 ((closure _.3 ((x))) . _.4) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 x))
                      (eval-expo
                       _.15
                       ((closure _.3 ((x))) . _.4)
                       (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 x))
                      (eval-expo
                       _.18
                       ((closure _.3 ((x))) . _.4)
                       (_.20 . _.19)))))))))))
             (conj
              (disj
               (disj
                (conj
                 (disj
                  (pause
                   (state ((lambda (list '1 (app _.0 _.1) . _.2))))
                   (conj
                    (== (list . _.0) _.1)
                    (eval-listo _.0 ((x)) (closure _.2 _.3))))
                  (pause
                   (state ((lambda (list '1 (app _.0 _.1) . _.2))))
                   (disj
                    (conj
                     (== (var _.0) _.1)
                     (lookupo _.0 ((x)) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.4 _.5) _.1)
                        (eval-expo _.4 ((x)) (closure _.6 _.7)))
                       (eval-expo _.5 ((x)) _.8))
                      (eval-expo _.6 (_.8 . _.7) (closure _.2 _.3)))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (cons _.9 _.10) _.1)
                         (== (_.11 . _.12) (closure _.2 _.3)))
                        (eval-expo _.9 ((x)) _.11))
                       (eval-expo _.10 ((x)) _.12))
                      (disj
                       (conj
                        (conj (== (car _.13) _.1) (== _.14 (closure _.2 _.3)))
                        (eval-expo _.13 ((x)) (_.14 . _.15)))
                       (conj
                        (conj (== (cdr _.16) _.1) (== _.17 (closure _.2 _.3)))
                        (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                 (eval-expo _.0 (_.1 . _.2) _.3))
                (pause
                 (state
                  ((lambda (list '1 (app '(closure _.0 _.1) _.2) . _.3))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 _.2))
                  (disj
                   (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                   (disj
                    (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.5 _.6) _.1)
                        (eval-expo _.5 ((x)) (closure _.7 _.8)))
                       (eval-expo _.6 ((x)) _.9))
                      (eval-expo _.7 (_.9 . _.8) _.2))
                     (disj
                      (conj
                       (conj
                        (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) _.2))
                        (eval-expo _.10 ((x)) _.12))
                       (eval-expo _.11 ((x)) _.13))
                      (disj
                       (conj
                        (conj (== (car _.14) _.1) (== _.15 _.2))
                        (eval-expo _.14 ((x)) (_.15 . _.16)))
                       (conj
                        (conj (== (cdr _.17) _.1) (== _.18 _.2))
                        (eval-expo _.17 ((x)) (_.19 . _.18)))))))))))
               (disj
                (pause
                 (state ((lambda (list '1 (app (lambda _.0) _.1) . _.2))))
                 (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2)))
                (pause
                 (state ((lambda (list '1 (app (lambda _.0) _.1) . _.2))))
                 (disj
                  (conj (== (var _.0) _.1) (lookupo _.0 ((x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.3 _.4) _.1)
                      (eval-expo _.3 ((x)) (closure _.5 _.6)))
                     (eval-expo _.4 ((x)) _.7))
                    (eval-expo _.5 (_.7 . _.6) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) _.2))
                      (eval-expo _.8 ((x)) _.10))
                     (eval-expo _.9 ((x)) _.11))
                    (disj
                     (conj
                      (conj (== (car _.12) _.1) (== _.13 _.2))
                      (eval-expo _.12 ((x)) (_.13 . _.14)))
                     (conj
                      (conj (== (cdr _.15) _.1) (== _.16 _.2))
                      (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
              (eval-expo _.0 (_.1 . _.2) _.3)))
            (disj
             (pause
              (state ((lambda (list '1 (app (lambda _.0) '_.1) . _.2))))
              (conj (== '_.0 _.1) (== _.0 x)))
             (pause
              (state ((lambda (list '1 (app (lambda _.0) '_.1) . _.2))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 (_.2 (x)) x))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 (_.2 (x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 (_.2 (x)) (closure _.6 _.7)))
                   (eval-expo _.5 (_.2 (x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) x))
                    (eval-expo _.9 (_.2 (x)) _.11))
                   (eval-expo _.10 (_.2 (x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 x))
                    (eval-expo _.13 (_.2 (x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 x))
                    (eval-expo _.16 (_.2 (x)) (_.18 . _.17)))))))))))
           (disj
            (pause
             (state
              ((lambda (list
                        '1
                        (app (lambda (list . _.0)) (lambda _.1))
                        .
                        _.2))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) x))
               (eval-expo _.0 ((closure _.5 ((x))) (x)) _.3))
              (eval-listo _.1 ((closure _.5 ((x))) (x)) _.4)))
            (disj
             (pause
              (state
               ((lambda (list '1 (app (lambda _.0) (lambda _.1)) . _.2))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((closure _.3 ((x))) (x)) (closure _.4 _.5)))
                 (eval-expo _.1 ((closure _.3 ((x))) (x)) _.6))
                (eval-expo _.4 (_.6 . _.5) x))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) x))
                  (eval-expo _.7 ((closure _.3 ((x))) (x)) _.9))
                 (eval-expo _.8 ((closure _.3 ((x))) (x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 x))
                  (eval-expo _.11 ((closure _.3 ((x))) (x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 x))
                  (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.16 . _.15)))))))
             (disj
              (pause
               (state
                ((lambda (list
                          '1
                          (app (lambda (var _.0)) (lambda _.1))
                          .
                          _.2))))
               (conj (== () _.0) (== (closure _.1 ((x))) x)))
              (pause
               (state
                ((lambda (list
                          '1
                          (app (lambda (var _.0)) (lambda _.1))
                          .
                          _.2))))
               (conj (== (s . _.0) _.1) (lookupo _.0 ((x)) x)))))))
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (list '1 (cdr _.0) . _.1))))
              (conj (== (var _.0) _.1) (lookupo _.0 ((x)) (_.2 . x))))
             (pause
              (state ((lambda (list '1 (cdr _.0) . _.1))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((x)) (closure _.3 _.4)))
                 (eval-expo _.1 ((x)) _.5))
                (eval-expo _.3 (_.5 . _.4) (_.6 . x)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) (_.6 . x)))
                  (eval-expo _.7 ((x)) _.9))
                 (eval-expo _.8 ((x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 (_.6 . x)))
                  (eval-expo _.11 ((x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 (_.6 . x)))
                  (eval-expo _.14 ((x)) (_.16 . _.15))))))))
            (pause
             (state ((lambda (list '1 (cdr (list . _.0)) . _.1))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (_.5 . x)))
               (eval-expo _.0 ((x)) _.3))
              (eval-listo _.1 ((x)) _.4))))
           (disj
            (pause
             (state ((lambda (list '1 (car (list . _.0)) . _.1))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (x . _.5)))
               (eval-expo _.0 ((x)) _.3))
              (eval-listo _.1 ((x)) _.4)))
            (disj
             (pause
              (state ((lambda (list '1 (car _.0) . _.1))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((x)) (closure _.3 _.4)))
                 (eval-expo _.1 ((x)) _.5))
                (eval-expo _.3 (_.5 . _.4) (x . _.6)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) (x . _.6)))
                  (eval-expo _.7 ((x)) _.9))
                 (eval-expo _.8 ((x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 (x . _.6)))
                  (eval-expo _.11 ((x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 (x . _.6)))
                  (eval-expo _.14 ((x)) (_.16 . _.15)))))))
             (disj
              (pause
               (state ((lambda (list '1 (car (var _.0)) . _.1))))
               (conj (== () _.0) (== (x) (x . _.1))))
              (pause
               (state ((lambda (list '1 (car (var _.0)) . _.1))))
               (conj (== (s . _.0) _.1) (lookupo _.0 () (x . _.2)))))))))
         (eval-listo _.0 (_.1 . _.2) _.3)))
       (disj
        (disj
         (disj
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (cdr _.0))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 ((x)) (closure _.3 _.4)))
                (eval-expo _.1 ((x)) _.5))
               (eval-expo _.3 (_.5 . _.4) (_.6 1 x))))
             (pause
              (state ((lambda (cdr _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (_.5 1 x)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 (_.5 1 x)))
                 (eval-expo _.6 ((x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 (_.5 1 x)))
                 (eval-expo _.9 ((x)) (_.11 . _.10)))))))
            (pause
             (state ((lambda (cdr (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () (_.2 1 x)))))
           (disj
            (disj
             (pause
              (state ((lambda (cdr (list (lambda _.0) . _.1)))))
              (conj (== () _.0) (== () (1 x))))
             (pause
              (state ((lambda (cdr (list (lambda _.0) . _.1)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (1 x)))
                (eval-expo _.0 ((x)) _.3))
               (eval-listo _.1 ((x)) _.4))))
            (conj
             (disj
              (pause
               (state ((lambda (cdr (list _.0 . _.1)))))
               (conj (== '_.0 _.1) (== _.0 _.2)))
              (pause
               (state ((lambda (cdr (list _.0 . _.1)))))
               (disj
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                (disj
                 (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.4 _.5) _.1)
                     (eval-expo _.4 ((x)) (closure _.6 _.7)))
                    (eval-expo _.5 ((x)) _.8))
                   (eval-expo _.6 (_.8 . _.7) _.2))
                  (disj
                   (conj
                    (conj
                     (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                     (eval-expo _.9 ((x)) _.11))
                    (eval-expo _.10 ((x)) _.12))
                   (disj
                    (conj
                     (conj (== (car _.13) _.1) (== _.14 _.2))
                     (eval-expo _.13 ((x)) (_.14 . _.15)))
                    (conj
                     (conj (== (cdr _.16) _.1) (== _.17 _.2))
                     (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
             (eval-listo _.0 (_.1 . _.2) _.3))))
          (disj
           (conj
            (disj
             (pause
              (state ((lambda (car (list _.0 . _.1)))))
              (conj (== '_.0 _.1) (== _.0 (1 x))))
             (pause
              (state ((lambda (car (list _.0 . _.1)))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (1 x)))
               (disj
                (conj (== (var _.2) _.1) (lookupo _.2 ((x)) (1 x)))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.3 _.4) _.1)
                    (eval-expo _.3 ((x)) (closure _.5 _.6)))
                   (eval-expo _.4 ((x)) _.7))
                  (eval-expo _.5 (_.7 . _.6) (1 x)))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) (1 x)))
                    (eval-expo _.8 ((x)) _.10))
                   (eval-expo _.9 ((x)) _.11))
                  (disj
                   (conj
                    (conj (== (car _.12) _.1) (== _.13 (1 x)))
                    (eval-expo _.12 ((x)) (_.13 . _.14)))
                   (conj
                    (conj (== (cdr _.15) _.1) (== _.16 (1 x)))
                    (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (disj
            (pause
             (state ((lambda (car (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () ((1 x) . _.2))))
            (disj
             (pause
              (state ((lambda (car _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) ((1 x) . _.5)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 ((1 x) . _.5)))
                 (eval-expo _.6 ((x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 ((1 x) . _.5)))
                 (eval-expo _.9 ((x)) (_.11 . _.10))))))
             (conj
              (conj
               (disj
                (pause
                 (state ((lambda (car (app _.0 _.1)))))
                 (conj
                  (== (lambda _.0) _.1)
                  (== (closure _.0 ((x))) (closure _.2 _.3))))
                (pause
                 (state ((lambda (car (app _.0 _.1)))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3)))
                  (disj
                   (conj
                    (== (list . _.4) _.1)
                    (eval-listo _.4 ((x)) (closure _.2 _.3)))
                   (disj
                    (conj
                     (== (var _.5) _.1)
                     (lookupo _.5 ((x)) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.6 _.7) _.1)
                        (eval-expo _.6 ((x)) (closure _.8 _.9)))
                       (eval-expo _.7 ((x)) _.10))
                      (eval-expo _.8 (_.10 . _.9) (closure _.2 _.3)))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (cons _.11 _.12) _.1)
                         (== (_.13 . _.14) (closure _.2 _.3)))
                        (eval-expo _.11 ((x)) _.13))
                       (eval-expo _.12 ((x)) _.14))
                      (disj
                       (conj
                        (conj (== (car _.15) _.1) (== _.16 (closure _.2 _.3)))
                        (eval-expo _.15 ((x)) (_.16 . _.17)))
                       (conj
                        (conj (== (cdr _.18) _.1) (== _.19 (closure _.2 _.3)))
                        (eval-expo _.18 ((x)) (_.20 . _.19)))))))))))
               (eval-expo _.0 (_.1 . _.2) _.3))
              (eval-expo _.0 (_.1 . _.2) (_.3 . _.4)))))))
         (disj
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (cons '1 _.0))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 ((x)) (closure _.3 _.4)))
                (eval-expo _.1 ((x)) _.5))
               (eval-expo _.3 (_.5 . _.4) (x))))
             (pause
              (state ((lambda (cons '1 _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (x)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.5) _.2) (== _.6 (x)))
                 (eval-expo _.5 ((x)) (_.6 . _.7)))
                (conj
                 (conj (== (cdr _.8) _.2) (== _.9 (x)))
                 (eval-expo _.8 ((x)) (_.10 . _.9)))))))
            (pause
             (state ((lambda (cons '1 (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () (x)))))
           (conj
            (disj
             (pause
              (state ((lambda (cons '1 (list _.0 . _.1)))))
              (conj (== '_.0 _.1) (== _.0 x)))
             (pause
              (state ((lambda (cons '1 (list _.0 . _.1)))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) x))
               (disj
                (conj (== (var _.2) _.1) (lookupo _.2 ((x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.3 _.4) _.1)
                    (eval-expo _.3 ((x)) (closure _.5 _.6)))
                   (eval-expo _.4 ((x)) _.7))
                  (eval-expo _.5 (_.7 . _.6) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) x))
                    (eval-expo _.8 ((x)) _.10))
                   (eval-expo _.9 ((x)) _.11))
                  (disj
                   (conj
                    (conj (== (car _.12) _.1) (== _.13 x))
                    (eval-expo _.12 ((x)) (_.13 . _.14)))
                   (conj
                    (conj (== (cdr _.15) _.1) (== _.16 x))
                    (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))
          (conj
           (disj
            (pause
             (state ((lambda (cons _.0 _.1))))
             (disj
              (conj
               (conj (== (car _.0) _.1) (== _.2 1))
               (eval-expo _.0 ((x)) (_.2 . _.3)))
              (conj
               (conj (== (cdr _.4) _.1) (== _.5 1))
               (eval-expo _.4 ((x)) (_.6 . _.5)))))
            (conj
             (disj
              (disj
               (pause
                (state ((lambda (cons (app (lambda _.0) _.1) _.2))))
                (conj (== (lambda _.0) _.1) (== (closure _.0 ((x))) _.2)))
               (pause
                (state ((lambda (cons (app (lambda _.0) _.1) _.2))))
                (disj
                 (conj (== '_.0 _.1) (== _.0 _.2))
                 (disj
                  (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                  (disj
                   (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (app _.5 _.6) _.1)
                       (eval-expo _.5 ((x)) (closure _.7 _.8)))
                      (eval-expo _.6 ((x)) _.9))
                     (eval-expo _.7 (_.9 . _.8) _.2))
                    (disj
                     (conj
                      (conj
                       (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) _.2))
                       (eval-expo _.10 ((x)) _.12))
                      (eval-expo _.11 ((x)) _.13))
                     (disj
                      (conj
                       (conj (== (car _.14) _.1) (== _.15 _.2))
                       (eval-expo _.14 ((x)) (_.15 . _.16)))
                      (conj
                       (conj (== (cdr _.17) _.1) (== _.18 _.2))
                       (eval-expo _.17 ((x)) (_.19 . _.18)))))))))))
              (conj
               (disj
                (pause
                 (state ((lambda (cons (app _.0 _.1) _.2))))
                 (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3))))
                (pause
                 (state ((lambda (cons (app _.0 _.1) _.2))))
                 (disj
                  (conj
                   (== (list . _.0) _.1)
                   (eval-listo _.0 ((x)) (closure _.2 _.3)))
                  (disj
                   (conj
                    (== (var _.4) _.1)
                    (lookupo _.4 ((x)) (closure _.2 _.3)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (app _.5 _.6) _.1)
                       (eval-expo _.5 ((x)) (closure _.7 _.8)))
                      (eval-expo _.6 ((x)) _.9))
                     (eval-expo _.7 (_.9 . _.8) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (cons _.10 _.11) _.1)
                        (== (_.12 . _.13) (closure _.2 _.3)))
                       (eval-expo _.10 ((x)) _.12))
                      (eval-expo _.11 ((x)) _.13))
                     (disj
                      (conj
                       (conj (== (car _.14) _.1) (== _.15 (closure _.2 _.3)))
                       (eval-expo _.14 ((x)) (_.15 . _.16)))
                      (conj
                       (conj (== (cdr _.17) _.1) (== _.18 (closure _.2 _.3)))
                       (eval-expo _.17 ((x)) (_.19 . _.18))))))))))
               (eval-expo _.0 (_.1 . _.2) _.3)))
             (eval-expo _.0 (_.1 . _.2) _.3)))
           (eval-expo _.0 (_.1 . _.2) _.3))))
        (disj
         (disj
          (disj
           (disj
            (pause
             (state ((lambda (app (lambda _.0) (lambda _.1)))))
             (disj
              (conj
               (conj (== (car _.0) _.1) (== _.2 (1 x)))
               (eval-expo _.0 ((closure _.3 ((x))) (x)) (_.2 . _.4)))
              (conj
               (conj (== (cdr _.5) _.1) (== _.6 (1 x)))
               (eval-expo _.5 ((closure _.3 ((x))) (x)) (_.7 . _.6)))))
            (conj
             (disj
              (pause
               (state ((lambda (app (lambda (cons _.0 _.1)) (lambda _.2)))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) (x))) 1)))
              (pause
               (state ((lambda (app (lambda (cons _.0 _.1)) (lambda _.2)))))
               (disj
                (conj (== '_.0 _.1) (== _.0 1))
                (disj
                 (conj
                  (== (list . _.2) _.1)
                  (eval-listo _.2 ((closure _.3 ((x))) (x)) 1))
                 (disj
                  (conj
                   (== (var _.4) _.1)
                   (lookupo _.4 ((closure _.3 ((x))) (x)) 1))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.5 _.6) _.1)
                      (eval-expo
                       _.5
                       ((closure _.3 ((x))) (x))
                       (closure _.7 _.8)))
                     (eval-expo _.6 ((closure _.3 ((x))) (x)) _.9))
                    (eval-expo _.7 (_.9 . _.8) 1))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) 1))
                      (eval-expo _.10 ((closure _.3 ((x))) (x)) _.12))
                     (eval-expo _.11 ((closure _.3 ((x))) (x)) _.13))
                    (disj
                     (conj
                      (conj (== (car _.14) _.1) (== _.15 1))
                      (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.15 . _.16)))
                     (conj
                      (conj (== (cdr _.17) _.1) (== _.18 1))
                      (eval-expo
                       _.17
                       ((closure _.3 ((x))) (x))
                       (_.19 . _.18)))))))))))
             (eval-expo _.0 (_.1 . _.2) _.3)))
           (conj
            (disj
             (disj
              (pause
               (state
                ((lambda (app (lambda (app (lambda _.0) _.1)) (lambda _.2)))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) (x))) _.3)))
              (pause
               (state
                ((lambda (app (lambda (app (lambda _.0) _.1)) (lambda _.2)))))
               (disj
                (conj (== '_.0 _.1) (== _.0 _.2))
                (disj
                 (conj
                  (== (list . _.3) _.1)
                  (eval-listo _.3 ((closure _.4 ((x))) (x)) _.2))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.4 ((x))) (x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.4 ((x))) (x))
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.4 ((x))) (x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) _.2))
                      (eval-expo _.11 ((closure _.4 ((x))) (x)) _.13))
                     (eval-expo _.12 ((closure _.4 ((x))) (x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 _.2))
                      (eval-expo _.15 ((closure _.4 ((x))) (x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 _.2))
                      (eval-expo
                       _.18
                       ((closure _.4 ((x))) (x))
                       (_.20 . _.19)))))))))))
             (conj
              (disj
               (pause
                (state ((lambda (app (lambda (app _.0 _.1)) (lambda _.2)))))
                (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3))))
               (pause
                (state ((lambda (app (lambda (app _.0 _.1)) (lambda _.2)))))
                (disj
                 (conj
                  (== (list . _.0) _.1)
                  (eval-listo _.0 ((closure _.2 ((x))) (x)) (closure _.3 _.4)))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.2 ((x))) (x)) (closure _.3 _.4)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.2 ((x))) (x))
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.2 ((x))) (x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) (closure _.3 _.4)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.11 _.12) _.1)
                       (== (_.13 . _.14) (closure _.3 _.4)))
                      (eval-expo _.11 ((closure _.2 ((x))) (x)) _.13))
                     (eval-expo _.12 ((closure _.2 ((x))) (x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 (closure _.3 _.4)))
                      (eval-expo _.15 ((closure _.2 ((x))) (x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 (closure _.3 _.4)))
                      (eval-expo
                       _.18
                       ((closure _.2 ((x))) (x))
                       (_.20 . _.19))))))))))
              (eval-expo _.0 (_.1 . _.2) _.3)))
            (eval-expo _.0 (_.1 . _.2) _.3)))
          (disj
           (conj
            (pause
             (state ((lambda (app (lambda (list '1 _.0 . _.1)) (lambda _.2)))))
             (disj
              (conj (== '_.0 _.1) (== _.0 x))
              (disj
               (conj
                (== (list . _.2) _.1)
                (eval-listo _.2 ((closure _.3 ((x))) (x)) x))
               (disj
                (conj
                 (== (var _.4) _.1)
                 (lookupo _.4 ((closure _.3 ((x))) (x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.5 _.6) _.1)
                    (eval-expo
                     _.5
                     ((closure _.3 ((x))) (x))
                     (closure _.7 _.8)))
                   (eval-expo _.6 ((closure _.3 ((x))) (x)) _.9))
                  (eval-expo _.7 (_.9 . _.8) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) x))
                    (eval-expo _.10 ((closure _.3 ((x))) (x)) _.12))
                   (eval-expo _.11 ((closure _.3 ((x))) (x)) _.13))
                  (disj
                   (conj
                    (conj (== (car _.14) _.1) (== _.15 x))
                    (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.15 . _.16)))
                   (conj
                    (conj (== (cdr _.17) _.1) (== _.18 x))
                    (eval-expo
                     _.17
                     ((closure _.3 ((x))) (x))
                     (_.19 . _.18))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (conj
            (disj
             (disj
              (pause
               (state ((lambda (app (lambda (list _.0 . _.1)) (lambda _.2)))))
               (conj
                (== (var _.0) _.1)
                (lookupo _.0 ((closure _.2 ((x))) (x)) 1)))
              (pause
               (state ((lambda (app (lambda (list _.0 . _.1)) (lambda _.2)))))
               (disj
                (conj
                 (conj
                  (conj
                   (== (app _.0 _.1) _.2)
                   (eval-expo _.0 ((closure _.3 ((x))) (x)) (closure _.4 _.5)))
                  (eval-expo _.1 ((closure _.3 ((x))) (x)) _.6))
                 (eval-expo _.4 (_.6 . _.5) 1))
                (disj
                 (conj
                  (conj
                   (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) 1))
                   (eval-expo _.7 ((closure _.3 ((x))) (x)) _.9))
                  (eval-expo _.8 ((closure _.3 ((x))) (x)) _.10))
                 (disj
                  (conj
                   (conj (== (car _.11) _.2) (== _.12 1))
                   (eval-expo _.11 ((closure _.3 ((x))) (x)) (_.12 . _.13)))
                  (conj
                   (conj (== (cdr _.14) _.2) (== _.15 1))
                   (eval-expo
                    _.14
                    ((closure _.3 ((x))) (x))
                    (_.16 . _.15))))))))
             (pause
              (state
               ((lambda (app
                         (lambda (list (list . _.0) . _.1))
                         (lambda _.2)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) 1))
                (eval-expo _.0 ((closure _.5 ((x))) (x)) _.3))
               (eval-listo _.1 ((closure _.5 ((x))) (x)) _.4))))
            (eval-listo _.0 (_.1 . _.2) _.3))))
         (disj
          (disj
           (conj
            (disj
             (disj
              (disj
               (pause
                (state ((lambda (app (lambda _.0) (list . _.1)))))
                (conj (== () _.0) (== () _.1)))
               (pause
                (state ((lambda (app (lambda _.0) (list . _.1)))))
                (conj
                 (conj
                  (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) _.5))
                  (eval-expo _.0 ((x)) _.3))
                 (eval-listo _.1 ((x)) _.4))))
              (disj
               (pause
                (state ((lambda (app (lambda _.0) _.1))))
                (conj (== (var _.0) _.1) (lookupo _.0 ((x)) _.2)))
               (pause
                (state ((lambda (app (lambda _.0) _.1))))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.0 _.1) _.2)
                    (eval-expo _.0 ((x)) (closure _.3 _.4)))
                   (eval-expo _.1 ((x)) _.5))
                  (eval-expo _.3 (_.5 . _.4) _.6))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) _.6))
                    (eval-expo _.7 ((x)) _.9))
                   (eval-expo _.8 ((x)) _.10))
                  (disj
                   (conj
                    (conj (== (car _.11) _.2) (== _.12 _.6))
                    (eval-expo _.11 ((x)) (_.12 . _.13)))
                   (conj
                    (conj (== (cdr _.14) _.2) (== _.15 _.6))
                    (eval-expo _.14 ((x)) (_.16 . _.15)))))))))
             (disj
              (disj
               (pause
                (state ((lambda (app '(closure _.0 _.1) _.2))))
                (conj (== '_.0 _.1) (== _.0 _.2)))
               (pause
                (state ((lambda (app '(closure _.0 _.1) _.2))))
                (disj
                 (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                 (disj
                  (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.4 _.5) _.1)
                      (eval-expo _.4 ((x)) (closure _.6 _.7)))
                     (eval-expo _.5 ((x)) _.8))
                    (eval-expo _.6 (_.8 . _.7) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                      (eval-expo _.9 ((x)) _.11))
                     (eval-expo _.10 ((x)) _.12))
                    (disj
                     (conj
                      (conj (== (car _.13) _.1) (== _.14 _.2))
                      (eval-expo _.13 ((x)) (_.14 . _.15)))
                     (conj
                      (conj (== (cdr _.16) _.1) (== _.17 _.2))
                      (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
              (conj
               (disj
                (disj
                 (pause
                  (state ((lambda (app (list . _.0) _.1))))
                  (conj (== () _.0) (== () (closure _.1 _.2))))
                 (pause
                  (state ((lambda (app (list . _.0) _.1))))
                  (conj
                   (conj
                    (conj
                     (== (_.0 . _.1) _.2)
                     (== (_.3 . _.4) (closure _.5 _.6)))
                    (eval-expo _.0 ((x)) _.3))
                   (eval-listo _.1 ((x)) _.4))))
                (disj
                 (pause
                  (state ((lambda (app _.0 _.1))))
                  (conj
                   (== (var _.0) _.1)
                   (lookupo _.0 ((x)) (closure _.2 _.3))))
                 (pause
                  (state ((lambda (app _.0 _.1))))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.0 _.1) _.2)
                      (eval-expo _.0 ((x)) (closure _.3 _.4)))
                     (eval-expo _.1 ((x)) _.5))
                    (eval-expo _.3 (_.5 . _.4) (closure _.6 _.7)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.8 _.9) _.2)
                       (== (_.10 . _.11) (closure _.6 _.7)))
                      (eval-expo _.8 ((x)) _.10))
                     (eval-expo _.9 ((x)) _.11))
                    (disj
                     (conj
                      (conj (== (car _.12) _.2) (== _.13 (closure _.6 _.7)))
                      (eval-expo _.12 ((x)) (_.13 . _.14)))
                     (conj
                      (conj (== (cdr _.15) _.2) (== _.16 (closure _.6 _.7)))
                      (eval-expo _.15 ((x)) (_.17 . _.16)))))))))
               (eval-expo _.0 (_.1 . _.2) _.3))))
            (eval-expo _.0 (_.1 . _.2) _.3))
           (disj
            (disj
             (pause
              (state ((lambda (app '(closure (list . _.0) _.1) (lambda _.2)))))
              (conj (== () _.0) (== () (1 x))))
             (pause
              (state ((lambda (app '(closure (list . _.0) _.1) (lambda _.2)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (1 x)))
                (eval-expo _.0 ((closure _.5 ((x))) . _.6) _.3))
               (eval-listo _.1 ((closure _.5 ((x))) . _.6) _.4))))
            (disj
             (pause
              (state ((lambda (app '(closure _.0 _.1) (lambda _.2)))))
              (conj
               (== (var _.0) _.1)
               (lookupo _.0 ((closure _.2 ((x))) . _.3) (1 x))))
             (pause
              (state ((lambda (app '(closure _.0 _.1) (lambda _.2)))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo
                   _.0
                   ((closure _.3 ((x))) . _.4)
                   (closure _.5 _.6)))
                 (eval-expo _.1 ((closure _.3 ((x))) . _.4) _.7))
                (eval-expo _.5 (_.7 . _.6) (1 x)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.8 _.9) _.2) (== (_.10 . _.11) (1 x)))
                  (eval-expo _.8 ((closure _.3 ((x))) . _.4) _.10))
                 (eval-expo _.9 ((closure _.3 ((x))) . _.4) _.11))
                (disj
                 (conj
                  (conj (== (car _.12) _.2) (== _.13 (1 x)))
                  (eval-expo _.12 ((closure _.3 ((x))) . _.4) (_.13 . _.14)))
                 (conj
                  (conj (== (cdr _.15) _.2) (== _.16 (1 x)))
                  (eval-expo
                   _.15
                   ((closure _.3 ((x))) . _.4)
                   (_.17 . _.16))))))))))
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (app (lambda _.0) '_.1))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 (_.3 (x)) (closure _.4 _.5)))
                (eval-expo _.1 (_.3 (x)) _.6))
               (eval-expo _.4 (_.6 . _.5) (1 x))))
             (pause
              (state ((lambda (app (lambda _.0) '_.1))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (1 x)))
                 (eval-expo _.0 (_.5 (x)) _.3))
                (eval-expo _.1 (_.5 (x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 (1 x)))
                 (eval-expo _.6 (_.5 (x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 (1 x)))
                 (eval-expo _.9 (_.5 (x)) (_.11 . _.10)))))))
            (pause
             (state ((lambda (app (lambda (var _.0)) '_.1))))
             (conj (== (s . _.0) _.1) (lookupo _.0 ((x)) (1 x)))))
           (conj
            (disj
             (pause
              (state ((lambda (app (lambda (list _.0 . _.1)) '_.2))))
              (conj (== '_.0 _.1) (== _.0 1)))
             (pause
              (state ((lambda (app (lambda (list _.0 . _.1)) '_.2))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 (_.2 (x)) 1))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 (_.2 (x)) 1))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 (_.2 (x)) (closure _.6 _.7)))
                   (eval-expo _.5 (_.2 (x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) 1))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) 1))
                    (eval-expo _.9 (_.2 (x)) _.11))
                   (eval-expo _.10 (_.2 (x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 1))
                    (eval-expo _.13 (_.2 (x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 1))
                    (eval-expo _.16 (_.2 (x)) (_.18 . _.17))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))))))
      (eval-listo _.0 () _.1))
     (conj
      (disj
       (conj
        (disj
         (disj
          (pause
           (state ((lambda (app (lambda (var ())) '(1 x)))))
           (conj (== (lambda _.0) '(y)) (== (closure _.0 ()) _.1)))
          (pause
           (state ((lambda (app (lambda (var ())) '(1 x)))))
           (disj
            (conj (== '_.0 '(y)) (== _.0 _.1))
            (disj
             (conj (== (list . _.2) '(y)) (eval-listo _.2 () _.1))
             (disj
              (conj (== (var _.3) '(y)) (lookupo _.3 () _.1))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.4 _.5) '(y))
                  (eval-expo _.4 () (closure _.6 _.7)))
                 (eval-expo _.5 () _.8))
                (eval-expo _.6 (_.8 . _.7) _.1))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.9 _.10) '(y)) (== (_.11 . _.12) _.1))
                  (eval-expo _.9 () _.11))
                 (eval-expo _.10 () _.12))
                (disj
                 (conj
                  (conj (== (car _.13) '(y)) (== _.14 _.1))
                  (eval-expo _.13 () (_.14 . _.15)))
                 (conj
                  (conj (== (cdr _.16) '(y)) (== _.17 _.1))
                  (eval-expo _.16 () (_.18 . _.17)))))))))))
         (conj
          (disj
           (pause
            (state ((lambda (app (lambda (var ())) '(1 x)))))
            (conj
             (== '_.0 (lambda (app (lambda (var ())) '(1 x))))
             (== _.0 (closure _.1 _.2))))
           (pause
            (state ((lambda (app (lambda (var ())) '(1 x)))))
            (disj
             (conj
              (== (list . _.0) (lambda (app (lambda (var ())) '(1 x))))
              (eval-listo _.0 () (closure _.1 _.2)))
             (disj
              (conj
               (== (var _.3) (lambda (app (lambda (var ())) '(1 x))))
               (lookupo _.3 () (closure _.1 _.2)))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.4 _.5) (lambda (app (lambda (var ())) '(1 x))))
                  (eval-expo _.4 () (closure _.6 _.7)))
                 (eval-expo _.5 () _.8))
                (eval-expo _.6 (_.8 . _.7) (closure _.1 _.2)))
               (disj
                (conj
                 (conj
                  (conj
                   (== (cons _.9 _.10) (lambda (app (lambda (var ())) '(1 x))))
                   (== (_.11 . _.12) (closure _.1 _.2)))
                  (eval-expo _.9 () _.11))
                 (eval-expo _.10 () _.12))
                (disj
                 (conj
                  (conj
                   (== (car _.13) (lambda (app (lambda (var ())) '(1 x))))
                   (== _.14 (closure _.1 _.2)))
                  (eval-expo _.13 () (_.14 . _.15)))
                 (conj
                  (conj
                   (== (cdr _.16) (lambda (app (lambda (var ())) '(1 x))))
                   (== _.17 (closure _.1 _.2)))
                  (eval-expo _.16 () (_.18 . _.17))))))))))
          (eval-expo _.0 () _.1)))
        (eval-expo _.0 (_.1 . _.2) _.3))
       (disj
        (pause
         (state ((lambda (app (lambda (var ())) '(1 x)))))
         (conj
          (conj
           (== (car _.0) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           (== _.1 (1 y)))
          (eval-expo _.0 () (_.1 . _.2))))
        (pause
         (state ((lambda (app (lambda (var ())) '(1 x)))))
         (conj
          (conj
           (== (cdr _.0) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           (== _.1 (1 y)))
          (eval-expo _.0 () (_.2 . _.1))))))
      (eval-listo _.0 () _.1)))
    (conj
     (disj
      (conj
       (disj
        (conj
         (disj
          (pause
           (state ((lambda (list (car '(1 . _.0)) 'x))))
           (conj
            (conj
             (conj
              (== (app _.0 _.1) (lambda (list (car '(1 . _.2)) 'x)))
              (eval-expo _.0 () (closure _.3 _.4)))
             (eval-expo _.1 () _.5))
            (eval-expo _.3 (_.5 . _.4) (closure _.6 _.7))))
          (pause
           (state ((lambda (list (car '(1 . _.0)) 'x))))
           (disj
            (conj
             (conj
              (conj
               (== (cons _.0 _.1) (lambda (list (car '(1 . _.2)) 'x)))
               (== (_.3 . _.4) (closure _.5 _.6)))
              (eval-expo _.0 () _.3))
             (eval-expo _.1 () _.4))
            (disj
             (conj
              (conj
               (== (car _.7) (lambda (list (car '(1 . _.2)) 'x)))
               (== _.8 (closure _.5 _.6)))
              (eval-expo _.7 () (_.8 . _.9)))
             (conj
              (conj
               (== (cdr _.10) (lambda (list (car '(1 . _.2)) 'x)))
               (== _.11 (closure _.5 _.6)))
              (eval-expo _.10 () (_.12 . _.11)))))))
         (eval-expo _.0 () _.1))
        (pause
         (state ((lambda (list (car '(1 . _.0)) 'x))))
         (disj
          (conj
           (conj
            (conj (== (app _.0 _.1) '(y)) (eval-expo _.0 () (closure _.2 _.3)))
            (eval-expo _.1 () _.4))
           (eval-expo _.2 (_.4 . _.3) _.5))
          (disj
           (conj
            (conj
             (conj (== (cons _.6 _.7) '(y)) (== (_.8 . _.9) _.5))
             (eval-expo _.6 () _.8))
            (eval-expo _.7 () _.9))
           (disj
            (conj
             (conj (== (car _.10) '(y)) (== _.11 _.5))
             (eval-expo _.10 () (_.11 . _.12)))
            (conj
             (conj (== (cdr _.13) '(y)) (== _.14 _.5))
             (eval-expo _.13 () (_.15 . _.14))))))))
       (eval-expo _.0 (_.1 . _.2) _.3))
      (disj
       (pause
        (state ((lambda (list (car '(1 . _.0)) 'x))))
        (conj
         (conj
          (conj (== (_.0 . _.1) ((car '(1 . _.2)) 'x)) (== (_.3 . _.4) (1 y)))
          (eval-expo _.0 ((y)) _.3))
         (eval-listo _.1 ((y)) _.4)))
       (pause
        (state ((lambda (list (car '(1 . _.0)) 'x))))
        (disj
         (conj
          (conj
           (conj
            (== (app _.0 _.1) (list (car '(1 . _.2)) 'x))
            (eval-expo _.0 ((y)) (closure _.3 _.4)))
           (eval-expo _.1 ((y)) _.5))
          (eval-expo _.3 (_.5 . _.4) (1 y)))
         (disj
          (conj
           (conj
            (conj
             (== (cons _.6 _.7) (list (car '(1 . _.2)) 'x))
             (== (_.8 . _.9) (1 y)))
            (eval-expo _.6 ((y)) _.8))
           (eval-expo _.7 ((y)) _.9))
          (disj
           (conj
            (conj (== (car _.10) (list (car '(1 . _.2)) 'x)) (== _.11 (1 y)))
            (eval-expo _.10 ((y)) (_.11 . _.12)))
           (conj
            (conj (== (cdr _.13) (list (car '(1 . _.2)) 'x)) (== _.14 (1 y)))
            (eval-expo _.13 ((y)) (_.15 . _.14)))))))))
     (eval-listo _.0 () _.1)))))


;; (stream-pretty (step 1608 q-1p))
;; Snapshot containing the first answer: (lambda (cons '1 (var ())))

'((((lambda (cons '1 (var ())))))
  (disj
   (disj
    (disj
     (conj
      (disj
       (disj
        (disj
         (disj
          (disj
           (conj
            (disj
             (disj
              (pause
               (state ((lambda (list (cdr _.0) . _.1))))
               (disj
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (_.2 . 1)))
                (disj
                 (conj (== (var _.3) _.1) (lookupo _.3 ((x)) (_.2 . 1)))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.4 _.5) _.1)
                     (eval-expo _.4 ((x)) (closure _.6 _.7)))
                    (eval-expo _.5 ((x)) _.8))
                   (eval-expo _.6 (_.8 . _.7) (_.2 . 1)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (cons _.9 _.10) _.1)
                      (== (_.11 . _.12) (_.2 . 1)))
                     (eval-expo _.9 ((x)) _.11))
                    (eval-expo _.10 ((x)) _.12))
                   (disj
                    (conj
                     (conj (== (car _.13) _.1) (== _.14 (_.2 . 1)))
                     (eval-expo _.13 ((x)) (_.14 . _.15)))
                    (conj
                     (conj (== (cdr _.16) _.1) (== _.17 (_.2 . 1)))
                     (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
              (disj
               (pause
                (state ((lambda (list (car _.0) . _.1))))
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (1 . _.2))))
               (pause
                (state ((lambda (list (car _.0) . _.1))))
                (disj
                 (conj (== (var _.0) _.1) (lookupo _.0 ((x)) (1 . _.2)))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.3 _.4) _.1)
                     (eval-expo _.3 ((x)) (closure _.5 _.6)))
                    (eval-expo _.4 ((x)) _.7))
                   (eval-expo _.5 (_.7 . _.6) (1 . _.2)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (cons _.8 _.9) _.1)
                      (== (_.10 . _.11) (1 . _.2)))
                     (eval-expo _.8 ((x)) _.10))
                    (eval-expo _.9 ((x)) _.11))
                   (disj
                    (conj
                     (conj (== (car _.12) _.1) (== _.13 (1 . _.2)))
                     (eval-expo _.12 ((x)) (_.13 . _.14)))
                    (conj
                     (conj (== (cdr _.15) _.1) (== _.16 (1 . _.2)))
                     (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
             (disj
              (disj
               (pause
                (state ((lambda (list (app (lambda _.0) (lambda _.1)) . _.2))))
                (conj
                 (== (list . _.0) _.1)
                 (eval-listo _.0 ((closure _.2 ((x))) (x)) 1)))
               (pause
                (state ((lambda (list (app (lambda _.0) (lambda _.1)) . _.2))))
                (disj
                 (conj
                  (== (var _.0) _.1)
                  (lookupo _.0 ((closure _.2 ((x))) (x)) 1))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.3 _.4) _.1)
                     (eval-expo
                      _.3
                      ((closure _.2 ((x))) (x))
                      (closure _.5 _.6)))
                    (eval-expo _.4 ((closure _.2 ((x))) (x)) _.7))
                   (eval-expo _.5 (_.7 . _.6) 1))
                  (disj
                   (conj
                    (conj
                     (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) 1))
                     (eval-expo _.8 ((closure _.2 ((x))) (x)) _.10))
                    (eval-expo _.9 ((closure _.2 ((x))) (x)) _.11))
                   (disj
                    (conj
                     (conj (== (car _.12) _.1) (== _.13 1))
                     (eval-expo _.12 ((closure _.2 ((x))) (x)) (_.13 . _.14)))
                    (conj
                     (conj (== (cdr _.15) _.1) (== _.16 1))
                     (eval-expo
                      _.15
                      ((closure _.2 ((x))) (x))
                      (_.17 . _.16)))))))))
              (disj
               (disj
                (pause
                 (state ((lambda (list (app (lambda _.0) '_.1) . _.2))))
                 (conj (== (lambda _.0) _.1) (== (closure _.0 (_.2 (x))) 1)))
                (pause
                 (state ((lambda (list (app (lambda _.0) '_.1) . _.2))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 1))
                  (disj
                   (conj (== (list . _.2) _.1) (eval-listo _.2 (_.3 (x)) 1))
                   (disj
                    (conj (== (var _.4) _.1) (lookupo _.4 (_.3 (x)) 1))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.5 _.6) _.1)
                        (eval-expo _.5 (_.3 (x)) (closure _.7 _.8)))
                       (eval-expo _.6 (_.3 (x)) _.9))
                      (eval-expo _.7 (_.9 . _.8) 1))
                     (disj
                      (conj
                       (conj
                        (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) 1))
                        (eval-expo _.10 (_.3 (x)) _.12))
                       (eval-expo _.11 (_.3 (x)) _.13))
                      (disj
                       (conj
                        (conj (== (car _.14) _.1) (== _.15 1))
                        (eval-expo _.14 (_.3 (x)) (_.15 . _.16)))
                       (conj
                        (conj (== (cdr _.17) _.1) (== _.18 1))
                        (eval-expo _.17 (_.3 (x)) (_.19 . _.18)))))))))))
               (disj
                (conj
                 (disj
                  (pause
                   (state ((lambda (list (app (lambda _.0) _.1) . _.2))))
                   (disj
                    (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                    (disj
                     (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (app _.4 _.5) _.1)
                         (eval-expo _.4 ((x)) (closure _.6 _.7)))
                        (eval-expo _.5 ((x)) _.8))
                       (eval-expo _.6 (_.8 . _.7) _.2))
                      (disj
                       (conj
                        (conj
                         (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                         (eval-expo _.9 ((x)) _.11))
                        (eval-expo _.10 ((x)) _.12))
                       (disj
                        (conj
                         (conj (== (car _.13) _.1) (== _.14 _.2))
                         (eval-expo _.13 ((x)) (_.14 . _.15)))
                        (conj
                         (conj (== (cdr _.16) _.1) (== _.17 _.2))
                         (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                  (disj
                   (conj
                    (disj
                     (pause
                      (state ((lambda (list (app _.0 _.1) . _.2))))
                      (conj
                       (== (list . _.0) _.1)
                       (eval-listo _.0 ((x)) (closure _.2 _.3))))
                     (pause
                      (state ((lambda (list (app _.0 _.1) . _.2))))
                      (disj
                       (conj
                        (== (var _.0) _.1)
                        (lookupo _.0 ((x)) (closure _.2 _.3)))
                       (disj
                        (conj
                         (conj
                          (conj
                           (== (app _.4 _.5) _.1)
                           (eval-expo _.4 ((x)) (closure _.6 _.7)))
                          (eval-expo _.5 ((x)) _.8))
                         (eval-expo _.6 (_.8 . _.7) (closure _.2 _.3)))
                        (disj
                         (conj
                          (conj
                           (conj
                            (== (cons _.9 _.10) _.1)
                            (== (_.11 . _.12) (closure _.2 _.3)))
                           (eval-expo _.9 ((x)) _.11))
                          (eval-expo _.10 ((x)) _.12))
                         (disj
                          (conj
                           (conj
                            (== (car _.13) _.1)
                            (== _.14 (closure _.2 _.3)))
                           (eval-expo _.13 ((x)) (_.14 . _.15)))
                          (conj
                           (conj
                            (== (cdr _.16) _.1)
                            (== _.17 (closure _.2 _.3)))
                           (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                    (eval-expo _.0 (_.1 . _.2) _.3))
                   (pause
                    (state
                     ((lambda (list (app '(closure _.0 _.1) _.2) . _.3))))
                    (disj
                     (conj (== '_.0 _.1) (== _.0 _.2))
                     (disj
                      (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                      (disj
                       (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                       (disj
                        (conj
                         (conj
                          (conj
                           (== (app _.5 _.6) _.1)
                           (eval-expo _.5 ((x)) (closure _.7 _.8)))
                          (eval-expo _.6 ((x)) _.9))
                         (eval-expo _.7 (_.9 . _.8) _.2))
                        (disj
                         (conj
                          (conj
                           (conj
                            (== (cons _.10 _.11) _.1)
                            (== (_.12 . _.13) _.2))
                           (eval-expo _.10 ((x)) _.12))
                          (eval-expo _.11 ((x)) _.13))
                         (disj
                          (conj
                           (conj (== (car _.14) _.1) (== _.15 _.2))
                           (eval-expo _.14 ((x)) (_.15 . _.16)))
                          (conj
                           (conj (== (cdr _.17) _.1) (== _.18 _.2))
                           (eval-expo _.17 ((x)) (_.19 . _.18))))))))))))
                 (eval-expo _.0 (_.1 . _.2) _.3))
                (disj
                 (pause
                  (state
                   ((lambda (list
                             (app '(closure _.0 _.1) (lambda _.2))
                             .
                             _.3))))
                  (conj
                   (== (lambda _.0) _.1)
                   (== (closure _.0 ((closure _.2 ((x))) . _.3)) 1)))
                 (pause
                  (state
                   ((lambda (list
                             (app '(closure _.0 _.1) (lambda _.2))
                             .
                             _.3))))
                  (disj
                   (conj (== '_.0 _.1) (== _.0 1))
                   (disj
                    (conj
                     (== (list . _.2) _.1)
                     (eval-listo _.2 ((closure _.3 ((x))) . _.4) 1))
                    (disj
                     (conj
                      (== (var _.5) _.1)
                      (lookupo _.5 ((closure _.3 ((x))) . _.4) 1))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (app _.6 _.7) _.1)
                         (eval-expo
                          _.6
                          ((closure _.3 ((x))) . _.4)
                          (closure _.8 _.9)))
                        (eval-expo _.7 ((closure _.3 ((x))) . _.4) _.10))
                       (eval-expo _.8 (_.10 . _.9) 1))
                      (disj
                       (conj
                        (conj
                         (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) 1))
                         (eval-expo _.11 ((closure _.3 ((x))) . _.4) _.13))
                        (eval-expo _.12 ((closure _.3 ((x))) . _.4) _.14))
                       (disj
                        (conj
                         (conj (== (car _.15) _.1) (== _.16 1))
                         (eval-expo
                          _.15
                          ((closure _.3 ((x))) . _.4)
                          (_.16 . _.17)))
                        (conj
                         (conj (== (cdr _.18) _.1) (== _.19 1))
                         (eval-expo
                          _.18
                          ((closure _.3 ((x))) . _.4)
                          (_.20 . _.19)))))))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (conj
            (pause
             (state ((lambda (list (cdr '(_.0 . 1)) _.1 . _.2))))
             (disj
              (conj (== '_.0 _.1) (== _.0 x))
              (disj
               (conj (== (list . _.2) _.1) (eval-listo _.2 ((x)) x))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 ((x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 ((x)) (closure _.6 _.7)))
                   (eval-expo _.5 ((x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) x))
                    (eval-expo _.9 ((x)) _.11))
                   (eval-expo _.10 ((x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 x))
                    (eval-expo _.13 ((x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 x))
                    (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))
          (disj
           (pause
            (state ((lambda (list (car '(1 . _.0)) 'x . _.1))))
            (conj
             (conj
              (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) ()))
              (eval-expo _.0 ((x)) _.3))
             (eval-listo _.1 ((x)) _.4)))
           (conj
            (disj
             (pause
              (state ((lambda (list (car '(1 . _.0)) _.1 . _.2))))
              (disj
               (conj (== (var _.0) _.1) (lookupo _.0 ((x)) x))
               (disj
                (conj
                 (conj
                  (conj
                   (== (app _.2 _.3) _.1)
                   (eval-expo _.2 ((x)) (closure _.4 _.5)))
                  (eval-expo _.3 ((x)) _.6))
                 (eval-expo _.4 (_.6 . _.5) x))
                (disj
                 (conj
                  (conj
                   (conj (== (cons _.7 _.8) _.1) (== (_.9 . _.10) x))
                   (eval-expo _.7 ((x)) _.9))
                  (eval-expo _.8 ((x)) _.10))
                 (disj
                  (conj
                   (conj (== (car _.11) _.1) (== _.12 x))
                   (eval-expo _.11 ((x)) (_.12 . _.13)))
                  (conj
                   (conj (== (cdr _.14) _.1) (== _.15 x))
                   (eval-expo _.14 ((x)) (_.16 . _.15))))))))
             (disj
              (pause
               (state ((lambda (list (car '(1 . _.0)) (list . _.1) . _.2))))
               (conj (== () _.0) (== () x)))
              (pause
               (state ((lambda (list (car '(1 . _.0)) (list . _.1) . _.2))))
               (conj
                (conj
                 (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) x))
                 (eval-expo _.0 ((x)) _.3))
                (eval-listo _.1 ((x)) _.4)))))
            (eval-listo _.0 (_.1 . _.2) _.3))))
         (conj
          (disj
           (pause
            (state ((lambda (list (app (lambda '1) (lambda _.0)) _.1 . _.2))))
            (disj
             (conj
              (conj
               (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) x))
               (eval-expo _.0 ((x)) _.3))
              (eval-expo _.1 ((x)) _.4))
             (disj
              (conj
               (conj (== (car _.5) _.2) (== _.6 x))
               (eval-expo _.5 ((x)) (_.6 . _.7)))
              (conj
               (conj (== (cdr _.8) _.2) (== _.9 x))
               (eval-expo _.8 ((x)) (_.10 . _.9))))))
           (conj
            (conj
             (disj
              (pause
               (state
                ((lambda (list
                          (app (lambda '1) (lambda _.0))
                          (app _.1 _.2)
                          .
                          _.3))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((x))) (closure _.2 _.3))))
              (pause
               (state
                ((lambda (list
                          (app (lambda '1) (lambda _.0))
                          (app _.1 _.2)
                          .
                          _.3))))
               (disj
                (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3)))
                (disj
                 (conj
                  (== (list . _.4) _.1)
                  (eval-listo _.4 ((x)) (closure _.2 _.3)))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((x)) (closure _.2 _.3)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo _.6 ((x)) (closure _.8 _.9)))
                     (eval-expo _.7 ((x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) (closure _.2 _.3)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.11 _.12) _.1)
                       (== (_.13 . _.14) (closure _.2 _.3)))
                      (eval-expo _.11 ((x)) _.13))
                     (eval-expo _.12 ((x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 (closure _.2 _.3)))
                      (eval-expo _.15 ((x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 (closure _.2 _.3)))
                      (eval-expo _.18 ((x)) (_.20 . _.19)))))))))))
             (eval-expo _.0 (_.1 . _.2) _.3))
            (eval-expo _.0 (_.1 . _.2) _.3)))
          (eval-listo _.0 (_.1 . _.2) _.3)))
        (conj
         (disj
          (disj
           (disj
            (disj
             (disj
              (pause
               (state
                ((lambda (list
                          '1
                          (app '(closure _.0 _.1) (lambda _.2))
                          .
                          _.3))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) . _.3)) x)))
              (pause
               (state
                ((lambda (list
                          '1
                          (app '(closure _.0 _.1) (lambda _.2))
                          .
                          _.3))))
               (disj
                (conj (== '_.0 _.1) (== _.0 x))
                (disj
                 (conj
                  (== (list . _.2) _.1)
                  (eval-listo _.2 ((closure _.3 ((x))) . _.4) x))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.3 ((x))) . _.4) x))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.3 ((x))) . _.4)
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.3 ((x))) . _.4) _.10))
                    (eval-expo _.8 (_.10 . _.9) x))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) x))
                      (eval-expo _.11 ((closure _.3 ((x))) . _.4) _.13))
                     (eval-expo _.12 ((closure _.3 ((x))) . _.4) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 x))
                      (eval-expo
                       _.15
                       ((closure _.3 ((x))) . _.4)
                       (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 x))
                      (eval-expo
                       _.18
                       ((closure _.3 ((x))) . _.4)
                       (_.20 . _.19)))))))))))
             (conj
              (disj
               (disj
                (conj
                 (disj
                  (pause
                   (state ((lambda (list '1 (app _.0 _.1) . _.2))))
                   (conj
                    (== (list . _.0) _.1)
                    (eval-listo _.0 ((x)) (closure _.2 _.3))))
                  (pause
                   (state ((lambda (list '1 (app _.0 _.1) . _.2))))
                   (disj
                    (conj
                     (== (var _.0) _.1)
                     (lookupo _.0 ((x)) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.4 _.5) _.1)
                        (eval-expo _.4 ((x)) (closure _.6 _.7)))
                       (eval-expo _.5 ((x)) _.8))
                      (eval-expo _.6 (_.8 . _.7) (closure _.2 _.3)))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (cons _.9 _.10) _.1)
                         (== (_.11 . _.12) (closure _.2 _.3)))
                        (eval-expo _.9 ((x)) _.11))
                       (eval-expo _.10 ((x)) _.12))
                      (disj
                       (conj
                        (conj (== (car _.13) _.1) (== _.14 (closure _.2 _.3)))
                        (eval-expo _.13 ((x)) (_.14 . _.15)))
                       (conj
                        (conj (== (cdr _.16) _.1) (== _.17 (closure _.2 _.3)))
                        (eval-expo _.16 ((x)) (_.18 . _.17)))))))))
                 (eval-expo _.0 (_.1 . _.2) _.3))
                (pause
                 (state
                  ((lambda (list '1 (app '(closure _.0 _.1) _.2) . _.3))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 _.2))
                  (disj
                   (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                   (disj
                    (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.5 _.6) _.1)
                        (eval-expo _.5 ((x)) (closure _.7 _.8)))
                       (eval-expo _.6 ((x)) _.9))
                      (eval-expo _.7 (_.9 . _.8) _.2))
                     (disj
                      (conj
                       (conj
                        (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) _.2))
                        (eval-expo _.10 ((x)) _.12))
                       (eval-expo _.11 ((x)) _.13))
                      (disj
                       (conj
                        (conj (== (car _.14) _.1) (== _.15 _.2))
                        (eval-expo _.14 ((x)) (_.15 . _.16)))
                       (conj
                        (conj (== (cdr _.17) _.1) (== _.18 _.2))
                        (eval-expo _.17 ((x)) (_.19 . _.18)))))))))))
               (disj
                (pause
                 (state ((lambda (list '1 (app (lambda _.0) _.1) . _.2))))
                 (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2)))
                (pause
                 (state ((lambda (list '1 (app (lambda _.0) _.1) . _.2))))
                 (disj
                  (conj (== (var _.0) _.1) (lookupo _.0 ((x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.3 _.4) _.1)
                      (eval-expo _.3 ((x)) (closure _.5 _.6)))
                     (eval-expo _.4 ((x)) _.7))
                    (eval-expo _.5 (_.7 . _.6) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) _.2))
                      (eval-expo _.8 ((x)) _.10))
                     (eval-expo _.9 ((x)) _.11))
                    (disj
                     (conj
                      (conj (== (car _.12) _.1) (== _.13 _.2))
                      (eval-expo _.12 ((x)) (_.13 . _.14)))
                     (conj
                      (conj (== (cdr _.15) _.1) (== _.16 _.2))
                      (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
              (eval-expo _.0 (_.1 . _.2) _.3)))
            (disj
             (pause
              (state ((lambda (list '1 (app (lambda _.0) '_.1) . _.2))))
              (conj (== '_.0 _.1) (== _.0 x)))
             (pause
              (state ((lambda (list '1 (app (lambda _.0) '_.1) . _.2))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 (_.2 (x)) x))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 (_.2 (x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 (_.2 (x)) (closure _.6 _.7)))
                   (eval-expo _.5 (_.2 (x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) x))
                    (eval-expo _.9 (_.2 (x)) _.11))
                   (eval-expo _.10 (_.2 (x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 x))
                    (eval-expo _.13 (_.2 (x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 x))
                    (eval-expo _.16 (_.2 (x)) (_.18 . _.17)))))))))))
           (disj
            (pause
             (state
              ((lambda (list
                        '1
                        (app (lambda (list . _.0)) (lambda _.1))
                        .
                        _.2))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) x))
               (eval-expo _.0 ((closure _.5 ((x))) (x)) _.3))
              (eval-listo _.1 ((closure _.5 ((x))) (x)) _.4)))
            (disj
             (pause
              (state
               ((lambda (list '1 (app (lambda _.0) (lambda _.1)) . _.2))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((closure _.3 ((x))) (x)) (closure _.4 _.5)))
                 (eval-expo _.1 ((closure _.3 ((x))) (x)) _.6))
                (eval-expo _.4 (_.6 . _.5) x))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) x))
                  (eval-expo _.7 ((closure _.3 ((x))) (x)) _.9))
                 (eval-expo _.8 ((closure _.3 ((x))) (x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 x))
                  (eval-expo _.11 ((closure _.3 ((x))) (x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 x))
                  (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.16 . _.15)))))))
             (disj
              (pause
               (state
                ((lambda (list
                          '1
                          (app (lambda (var _.0)) (lambda _.1))
                          .
                          _.2))))
               (conj (== () _.0) (== (closure _.1 ((x))) x)))
              (pause
               (state
                ((lambda (list
                          '1
                          (app (lambda (var _.0)) (lambda _.1))
                          .
                          _.2))))
               (conj (== (s . _.0) _.1) (lookupo _.0 ((x)) x)))))))
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (list '1 (cdr _.0) . _.1))))
              (conj (== (var _.0) _.1) (lookupo _.0 ((x)) (_.2 . x))))
             (pause
              (state ((lambda (list '1 (cdr _.0) . _.1))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((x)) (closure _.3 _.4)))
                 (eval-expo _.1 ((x)) _.5))
                (eval-expo _.3 (_.5 . _.4) (_.6 . x)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) (_.6 . x)))
                  (eval-expo _.7 ((x)) _.9))
                 (eval-expo _.8 ((x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 (_.6 . x)))
                  (eval-expo _.11 ((x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 (_.6 . x)))
                  (eval-expo _.14 ((x)) (_.16 . _.15))))))))
            (pause
             (state ((lambda (list '1 (cdr (list . _.0)) . _.1))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (_.5 . x)))
               (eval-expo _.0 ((x)) _.3))
              (eval-listo _.1 ((x)) _.4))))
           (disj
            (pause
             (state ((lambda (list '1 (car (list . _.0)) . _.1))))
             (conj
              (conj
               (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (x . _.5)))
               (eval-expo _.0 ((x)) _.3))
              (eval-listo _.1 ((x)) _.4)))
            (disj
             (pause
              (state ((lambda (list '1 (car _.0) . _.1))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo _.0 ((x)) (closure _.3 _.4)))
                 (eval-expo _.1 ((x)) _.5))
                (eval-expo _.3 (_.5 . _.4) (x . _.6)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) (x . _.6)))
                  (eval-expo _.7 ((x)) _.9))
                 (eval-expo _.8 ((x)) _.10))
                (disj
                 (conj
                  (conj (== (car _.11) _.2) (== _.12 (x . _.6)))
                  (eval-expo _.11 ((x)) (_.12 . _.13)))
                 (conj
                  (conj (== (cdr _.14) _.2) (== _.15 (x . _.6)))
                  (eval-expo _.14 ((x)) (_.16 . _.15)))))))
             (disj
              (pause
               (state ((lambda (list '1 (car (var _.0)) . _.1))))
               (conj (== () _.0) (== (x) (x . _.1))))
              (pause
               (state ((lambda (list '1 (car (var _.0)) . _.1))))
               (conj (== (s . _.0) _.1) (lookupo _.0 () (x . _.2)))))))))
         (eval-listo _.0 (_.1 . _.2) _.3)))
       (disj
        (disj
         (disj
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (cdr _.0))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 ((x)) (closure _.3 _.4)))
                (eval-expo _.1 ((x)) _.5))
               (eval-expo _.3 (_.5 . _.4) (_.6 1 x))))
             (pause
              (state ((lambda (cdr _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (_.5 1 x)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 (_.5 1 x)))
                 (eval-expo _.6 ((x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 (_.5 1 x)))
                 (eval-expo _.9 ((x)) (_.11 . _.10)))))))
            (pause
             (state ((lambda (cdr (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () (_.2 1 x)))))
           (disj
            (disj
             (pause
              (state ((lambda (cdr (list (lambda _.0) . _.1)))))
              (conj (== () _.0) (== () (1 x))))
             (pause
              (state ((lambda (cdr (list (lambda _.0) . _.1)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (1 x)))
                (eval-expo _.0 ((x)) _.3))
               (eval-listo _.1 ((x)) _.4))))
            (conj
             (disj
              (pause
               (state ((lambda (cdr (list _.0 . _.1)))))
               (conj (== '_.0 _.1) (== _.0 _.2)))
              (pause
               (state ((lambda (cdr (list _.0 . _.1)))))
               (disj
                (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                (disj
                 (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                 (disj
                  (conj
                   (conj
                    (conj
                     (== (app _.4 _.5) _.1)
                     (eval-expo _.4 ((x)) (closure _.6 _.7)))
                    (eval-expo _.5 ((x)) _.8))
                   (eval-expo _.6 (_.8 . _.7) _.2))
                  (disj
                   (conj
                    (conj
                     (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                     (eval-expo _.9 ((x)) _.11))
                    (eval-expo _.10 ((x)) _.12))
                   (disj
                    (conj
                     (conj (== (car _.13) _.1) (== _.14 _.2))
                     (eval-expo _.13 ((x)) (_.14 . _.15)))
                    (conj
                     (conj (== (cdr _.16) _.1) (== _.17 _.2))
                     (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
             (eval-listo _.0 (_.1 . _.2) _.3))))
          (disj
           (conj
            (disj
             (pause
              (state ((lambda (car (list _.0 . _.1)))))
              (conj (== '_.0 _.1) (== _.0 (1 x))))
             (pause
              (state ((lambda (car (list _.0 . _.1)))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) (1 x)))
               (disj
                (conj (== (var _.2) _.1) (lookupo _.2 ((x)) (1 x)))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.3 _.4) _.1)
                    (eval-expo _.3 ((x)) (closure _.5 _.6)))
                   (eval-expo _.4 ((x)) _.7))
                  (eval-expo _.5 (_.7 . _.6) (1 x)))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) (1 x)))
                    (eval-expo _.8 ((x)) _.10))
                   (eval-expo _.9 ((x)) _.11))
                  (disj
                   (conj
                    (conj (== (car _.12) _.1) (== _.13 (1 x)))
                    (eval-expo _.12 ((x)) (_.13 . _.14)))
                   (conj
                    (conj (== (cdr _.15) _.1) (== _.16 (1 x)))
                    (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (disj
            (pause
             (state ((lambda (car (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () ((1 x) . _.2))))
            (disj
             (pause
              (state ((lambda (car _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) ((1 x) . _.5)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 ((1 x) . _.5)))
                 (eval-expo _.6 ((x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 ((1 x) . _.5)))
                 (eval-expo _.9 ((x)) (_.11 . _.10))))))
             (conj
              (conj
               (disj
                (pause
                 (state ((lambda (car (app _.0 _.1)))))
                 (conj
                  (== (lambda _.0) _.1)
                  (== (closure _.0 ((x))) (closure _.2 _.3))))
                (pause
                 (state ((lambda (car (app _.0 _.1)))))
                 (disj
                  (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3)))
                  (disj
                   (conj
                    (== (list . _.4) _.1)
                    (eval-listo _.4 ((x)) (closure _.2 _.3)))
                   (disj
                    (conj
                     (== (var _.5) _.1)
                     (lookupo _.5 ((x)) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (app _.6 _.7) _.1)
                        (eval-expo _.6 ((x)) (closure _.8 _.9)))
                       (eval-expo _.7 ((x)) _.10))
                      (eval-expo _.8 (_.10 . _.9) (closure _.2 _.3)))
                     (disj
                      (conj
                       (conj
                        (conj
                         (== (cons _.11 _.12) _.1)
                         (== (_.13 . _.14) (closure _.2 _.3)))
                        (eval-expo _.11 ((x)) _.13))
                       (eval-expo _.12 ((x)) _.14))
                      (disj
                       (conj
                        (conj (== (car _.15) _.1) (== _.16 (closure _.2 _.3)))
                        (eval-expo _.15 ((x)) (_.16 . _.17)))
                       (conj
                        (conj (== (cdr _.18) _.1) (== _.19 (closure _.2 _.3)))
                        (eval-expo _.18 ((x)) (_.20 . _.19)))))))))))
               (eval-expo _.0 (_.1 . _.2) _.3))
              (eval-expo _.0 (_.1 . _.2) (_.3 . _.4)))))))
         (disj
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (cons '1 _.0))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 ((x)) (closure _.3 _.4)))
                (eval-expo _.1 ((x)) _.5))
               (eval-expo _.3 (_.5 . _.4) (x))))
             (pause
              (state ((lambda (cons '1 _.0))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (x)))
                 (eval-expo _.0 ((x)) _.3))
                (eval-expo _.1 ((x)) _.4))
               (disj
                (conj
                 (conj (== (car _.5) _.2) (== _.6 (x)))
                 (eval-expo _.5 ((x)) (_.6 . _.7)))
                (conj
                 (conj (== (cdr _.8) _.2) (== _.9 (x)))
                 (eval-expo _.8 ((x)) (_.10 . _.9)))))))
            (pause
             (state ((lambda (cons '1 (var _.0)))))
             (conj (== (s . _.0) _.1) (lookupo _.0 () (x)))))
           (conj
            (disj
             (pause
              (state ((lambda (cons '1 (list _.0 . _.1)))))
              (conj (== '_.0 _.1) (== _.0 x)))
             (pause
              (state ((lambda (cons '1 (list _.0 . _.1)))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) x))
               (disj
                (conj (== (var _.2) _.1) (lookupo _.2 ((x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.3 _.4) _.1)
                    (eval-expo _.3 ((x)) (closure _.5 _.6)))
                   (eval-expo _.4 ((x)) _.7))
                  (eval-expo _.5 (_.7 . _.6) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.8 _.9) _.1) (== (_.10 . _.11) x))
                    (eval-expo _.8 ((x)) _.10))
                   (eval-expo _.9 ((x)) _.11))
                  (disj
                   (conj
                    (conj (== (car _.12) _.1) (== _.13 x))
                    (eval-expo _.12 ((x)) (_.13 . _.14)))
                   (conj
                    (conj (== (cdr _.15) _.1) (== _.16 x))
                    (eval-expo _.15 ((x)) (_.17 . _.16))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))
          (conj
           (disj
            (pause
             (state ((lambda (cons _.0 _.1))))
             (disj
              (conj
               (conj (== (car _.0) _.1) (== _.2 1))
               (eval-expo _.0 ((x)) (_.2 . _.3)))
              (conj
               (conj (== (cdr _.4) _.1) (== _.5 1))
               (eval-expo _.4 ((x)) (_.6 . _.5)))))
            (conj
             (disj
              (disj
               (pause
                (state ((lambda (cons (app (lambda _.0) _.1) _.2))))
                (conj (== (lambda _.0) _.1) (== (closure _.0 ((x))) _.2)))
               (pause
                (state ((lambda (cons (app (lambda _.0) _.1) _.2))))
                (disj
                 (conj (== '_.0 _.1) (== _.0 _.2))
                 (disj
                  (conj (== (list . _.3) _.1) (eval-listo _.3 ((x)) _.2))
                  (disj
                   (conj (== (var _.4) _.1) (lookupo _.4 ((x)) _.2))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (app _.5 _.6) _.1)
                       (eval-expo _.5 ((x)) (closure _.7 _.8)))
                      (eval-expo _.6 ((x)) _.9))
                     (eval-expo _.7 (_.9 . _.8) _.2))
                    (disj
                     (conj
                      (conj
                       (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) _.2))
                       (eval-expo _.10 ((x)) _.12))
                      (eval-expo _.11 ((x)) _.13))
                     (disj
                      (conj
                       (conj (== (car _.14) _.1) (== _.15 _.2))
                       (eval-expo _.14 ((x)) (_.15 . _.16)))
                      (conj
                       (conj (== (cdr _.17) _.1) (== _.18 _.2))
                       (eval-expo _.17 ((x)) (_.19 . _.18)))))))))))
              (conj
               (disj
                (pause
                 (state ((lambda (cons (app _.0 _.1) _.2))))
                 (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3))))
                (pause
                 (state ((lambda (cons (app _.0 _.1) _.2))))
                 (disj
                  (conj
                   (== (list . _.0) _.1)
                   (eval-listo _.0 ((x)) (closure _.2 _.3)))
                  (disj
                   (conj
                    (== (var _.4) _.1)
                    (lookupo _.4 ((x)) (closure _.2 _.3)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (app _.5 _.6) _.1)
                       (eval-expo _.5 ((x)) (closure _.7 _.8)))
                      (eval-expo _.6 ((x)) _.9))
                     (eval-expo _.7 (_.9 . _.8) (closure _.2 _.3)))
                    (disj
                     (conj
                      (conj
                       (conj
                        (== (cons _.10 _.11) _.1)
                        (== (_.12 . _.13) (closure _.2 _.3)))
                       (eval-expo _.10 ((x)) _.12))
                      (eval-expo _.11 ((x)) _.13))
                     (disj
                      (conj
                       (conj (== (car _.14) _.1) (== _.15 (closure _.2 _.3)))
                       (eval-expo _.14 ((x)) (_.15 . _.16)))
                      (conj
                       (conj (== (cdr _.17) _.1) (== _.18 (closure _.2 _.3)))
                       (eval-expo _.17 ((x)) (_.19 . _.18))))))))))
               (eval-expo _.0 (_.1 . _.2) _.3)))
             (eval-expo _.0 (_.1 . _.2) _.3)))
           (eval-expo _.0 (_.1 . _.2) _.3))))
        (disj
         (disj
          (disj
           (disj
            (pause
             (state ((lambda (app (lambda _.0) (lambda _.1)))))
             (disj
              (conj
               (conj (== (car _.0) _.1) (== _.2 (1 x)))
               (eval-expo _.0 ((closure _.3 ((x))) (x)) (_.2 . _.4)))
              (conj
               (conj (== (cdr _.5) _.1) (== _.6 (1 x)))
               (eval-expo _.5 ((closure _.3 ((x))) (x)) (_.7 . _.6)))))
            (conj
             (disj
              (pause
               (state ((lambda (app (lambda (cons _.0 _.1)) (lambda _.2)))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) (x))) 1)))
              (pause
               (state ((lambda (app (lambda (cons _.0 _.1)) (lambda _.2)))))
               (disj
                (conj (== '_.0 _.1) (== _.0 1))
                (disj
                 (conj
                  (== (list . _.2) _.1)
                  (eval-listo _.2 ((closure _.3 ((x))) (x)) 1))
                 (disj
                  (conj
                   (== (var _.4) _.1)
                   (lookupo _.4 ((closure _.3 ((x))) (x)) 1))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.5 _.6) _.1)
                      (eval-expo
                       _.5
                       ((closure _.3 ((x))) (x))
                       (closure _.7 _.8)))
                     (eval-expo _.6 ((closure _.3 ((x))) (x)) _.9))
                    (eval-expo _.7 (_.9 . _.8) 1))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) 1))
                      (eval-expo _.10 ((closure _.3 ((x))) (x)) _.12))
                     (eval-expo _.11 ((closure _.3 ((x))) (x)) _.13))
                    (disj
                     (conj
                      (conj (== (car _.14) _.1) (== _.15 1))
                      (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.15 . _.16)))
                     (conj
                      (conj (== (cdr _.17) _.1) (== _.18 1))
                      (eval-expo
                       _.17
                       ((closure _.3 ((x))) (x))
                       (_.19 . _.18)))))))))))
             (eval-expo _.0 (_.1 . _.2) _.3)))
           (conj
            (disj
             (disj
              (pause
               (state
                ((lambda (app (lambda (app (lambda _.0) _.1)) (lambda _.2)))))
               (conj
                (== (lambda _.0) _.1)
                (== (closure _.0 ((closure _.2 ((x))) (x))) _.3)))
              (pause
               (state
                ((lambda (app (lambda (app (lambda _.0) _.1)) (lambda _.2)))))
               (disj
                (conj (== '_.0 _.1) (== _.0 _.2))
                (disj
                 (conj
                  (== (list . _.3) _.1)
                  (eval-listo _.3 ((closure _.4 ((x))) (x)) _.2))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.4 ((x))) (x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.4 ((x))) (x))
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.4 ((x))) (x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.11 _.12) _.1) (== (_.13 . _.14) _.2))
                      (eval-expo _.11 ((closure _.4 ((x))) (x)) _.13))
                     (eval-expo _.12 ((closure _.4 ((x))) (x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 _.2))
                      (eval-expo _.15 ((closure _.4 ((x))) (x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 _.2))
                      (eval-expo
                       _.18
                       ((closure _.4 ((x))) (x))
                       (_.20 . _.19)))))))))))
             (conj
              (disj
               (pause
                (state ((lambda (app (lambda (app _.0 _.1)) (lambda _.2)))))
                (conj (== '_.0 _.1) (== _.0 (closure _.2 _.3))))
               (pause
                (state ((lambda (app (lambda (app _.0 _.1)) (lambda _.2)))))
                (disj
                 (conj
                  (== (list . _.0) _.1)
                  (eval-listo _.0 ((closure _.2 ((x))) (x)) (closure _.3 _.4)))
                 (disj
                  (conj
                   (== (var _.5) _.1)
                   (lookupo _.5 ((closure _.2 ((x))) (x)) (closure _.3 _.4)))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.6 _.7) _.1)
                      (eval-expo
                       _.6
                       ((closure _.2 ((x))) (x))
                       (closure _.8 _.9)))
                     (eval-expo _.7 ((closure _.2 ((x))) (x)) _.10))
                    (eval-expo _.8 (_.10 . _.9) (closure _.3 _.4)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.11 _.12) _.1)
                       (== (_.13 . _.14) (closure _.3 _.4)))
                      (eval-expo _.11 ((closure _.2 ((x))) (x)) _.13))
                     (eval-expo _.12 ((closure _.2 ((x))) (x)) _.14))
                    (disj
                     (conj
                      (conj (== (car _.15) _.1) (== _.16 (closure _.3 _.4)))
                      (eval-expo _.15 ((closure _.2 ((x))) (x)) (_.16 . _.17)))
                     (conj
                      (conj (== (cdr _.18) _.1) (== _.19 (closure _.3 _.4)))
                      (eval-expo
                       _.18
                       ((closure _.2 ((x))) (x))
                       (_.20 . _.19))))))))))
              (eval-expo _.0 (_.1 . _.2) _.3)))
            (eval-expo _.0 (_.1 . _.2) _.3)))
          (disj
           (conj
            (pause
             (state ((lambda (app (lambda (list '1 _.0 . _.1)) (lambda _.2)))))
             (disj
              (conj (== '_.0 _.1) (== _.0 x))
              (disj
               (conj
                (== (list . _.2) _.1)
                (eval-listo _.2 ((closure _.3 ((x))) (x)) x))
               (disj
                (conj
                 (== (var _.4) _.1)
                 (lookupo _.4 ((closure _.3 ((x))) (x)) x))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.5 _.6) _.1)
                    (eval-expo
                     _.5
                     ((closure _.3 ((x))) (x))
                     (closure _.7 _.8)))
                   (eval-expo _.6 ((closure _.3 ((x))) (x)) _.9))
                  (eval-expo _.7 (_.9 . _.8) x))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.10 _.11) _.1) (== (_.12 . _.13) x))
                    (eval-expo _.10 ((closure _.3 ((x))) (x)) _.12))
                   (eval-expo _.11 ((closure _.3 ((x))) (x)) _.13))
                  (disj
                   (conj
                    (conj (== (car _.14) _.1) (== _.15 x))
                    (eval-expo _.14 ((closure _.3 ((x))) (x)) (_.15 . _.16)))
                   (conj
                    (conj (== (cdr _.17) _.1) (== _.18 x))
                    (eval-expo
                     _.17
                     ((closure _.3 ((x))) (x))
                     (_.19 . _.18))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3))
           (conj
            (disj
             (disj
              (pause
               (state ((lambda (app (lambda (list _.0 . _.1)) (lambda _.2)))))
               (conj
                (== (var _.0) _.1)
                (lookupo _.0 ((closure _.2 ((x))) (x)) 1)))
              (pause
               (state ((lambda (app (lambda (list _.0 . _.1)) (lambda _.2)))))
               (disj
                (conj
                 (conj
                  (conj
                   (== (app _.0 _.1) _.2)
                   (eval-expo _.0 ((closure _.3 ((x))) (x)) (closure _.4 _.5)))
                  (eval-expo _.1 ((closure _.3 ((x))) (x)) _.6))
                 (eval-expo _.4 (_.6 . _.5) 1))
                (disj
                 (conj
                  (conj
                   (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) 1))
                   (eval-expo _.7 ((closure _.3 ((x))) (x)) _.9))
                  (eval-expo _.8 ((closure _.3 ((x))) (x)) _.10))
                 (disj
                  (conj
                   (conj (== (car _.11) _.2) (== _.12 1))
                   (eval-expo _.11 ((closure _.3 ((x))) (x)) (_.12 . _.13)))
                  (conj
                   (conj (== (cdr _.14) _.2) (== _.15 1))
                   (eval-expo
                    _.14
                    ((closure _.3 ((x))) (x))
                    (_.16 . _.15))))))))
             (pause
              (state
               ((lambda (app
                         (lambda (list (list . _.0) . _.1))
                         (lambda _.2)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) 1))
                (eval-expo _.0 ((closure _.5 ((x))) (x)) _.3))
               (eval-listo _.1 ((closure _.5 ((x))) (x)) _.4))))
            (eval-listo _.0 (_.1 . _.2) _.3))))
         (disj
          (disj
           (conj
            (disj
             (disj
              (disj
               (pause
                (state ((lambda (app (lambda _.0) (list . _.1)))))
                (conj (== () _.0) (== () _.1)))
               (pause
                (state ((lambda (app (lambda _.0) (list . _.1)))))
                (conj
                 (conj
                  (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) _.5))
                  (eval-expo _.0 ((x)) _.3))
                 (eval-listo _.1 ((x)) _.4))))
              (disj
               (pause
                (state ((lambda (app (lambda _.0) _.1))))
                (conj (== (var _.0) _.1) (lookupo _.0 ((x)) _.2)))
               (pause
                (state ((lambda (app (lambda _.0) _.1))))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.0 _.1) _.2)
                    (eval-expo _.0 ((x)) (closure _.3 _.4)))
                   (eval-expo _.1 ((x)) _.5))
                  (eval-expo _.3 (_.5 . _.4) _.6))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.7 _.8) _.2) (== (_.9 . _.10) _.6))
                    (eval-expo _.7 ((x)) _.9))
                   (eval-expo _.8 ((x)) _.10))
                  (disj
                   (conj
                    (conj (== (car _.11) _.2) (== _.12 _.6))
                    (eval-expo _.11 ((x)) (_.12 . _.13)))
                   (conj
                    (conj (== (cdr _.14) _.2) (== _.15 _.6))
                    (eval-expo _.14 ((x)) (_.16 . _.15)))))))))
             (disj
              (disj
               (pause
                (state ((lambda (app '(closure _.0 _.1) _.2))))
                (conj (== '_.0 _.1) (== _.0 _.2)))
               (pause
                (state ((lambda (app '(closure _.0 _.1) _.2))))
                (disj
                 (conj (== (list . _.0) _.1) (eval-listo _.0 ((x)) _.2))
                 (disj
                  (conj (== (var _.3) _.1) (lookupo _.3 ((x)) _.2))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.4 _.5) _.1)
                      (eval-expo _.4 ((x)) (closure _.6 _.7)))
                     (eval-expo _.5 ((x)) _.8))
                    (eval-expo _.6 (_.8 . _.7) _.2))
                   (disj
                    (conj
                     (conj
                      (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) _.2))
                      (eval-expo _.9 ((x)) _.11))
                     (eval-expo _.10 ((x)) _.12))
                    (disj
                     (conj
                      (conj (== (car _.13) _.1) (== _.14 _.2))
                      (eval-expo _.13 ((x)) (_.14 . _.15)))
                     (conj
                      (conj (== (cdr _.16) _.1) (== _.17 _.2))
                      (eval-expo _.16 ((x)) (_.18 . _.17))))))))))
              (conj
               (disj
                (disj
                 (pause
                  (state ((lambda (app (list . _.0) _.1))))
                  (conj (== () _.0) (== () (closure _.1 _.2))))
                 (pause
                  (state ((lambda (app (list . _.0) _.1))))
                  (conj
                   (conj
                    (conj
                     (== (_.0 . _.1) _.2)
                     (== (_.3 . _.4) (closure _.5 _.6)))
                    (eval-expo _.0 ((x)) _.3))
                   (eval-listo _.1 ((x)) _.4))))
                (disj
                 (pause
                  (state ((lambda (app _.0 _.1))))
                  (conj
                   (== (var _.0) _.1)
                   (lookupo _.0 ((x)) (closure _.2 _.3))))
                 (pause
                  (state ((lambda (app _.0 _.1))))
                  (disj
                   (conj
                    (conj
                     (conj
                      (== (app _.0 _.1) _.2)
                      (eval-expo _.0 ((x)) (closure _.3 _.4)))
                     (eval-expo _.1 ((x)) _.5))
                    (eval-expo _.3 (_.5 . _.4) (closure _.6 _.7)))
                   (disj
                    (conj
                     (conj
                      (conj
                       (== (cons _.8 _.9) _.2)
                       (== (_.10 . _.11) (closure _.6 _.7)))
                      (eval-expo _.8 ((x)) _.10))
                     (eval-expo _.9 ((x)) _.11))
                    (disj
                     (conj
                      (conj (== (car _.12) _.2) (== _.13 (closure _.6 _.7)))
                      (eval-expo _.12 ((x)) (_.13 . _.14)))
                     (conj
                      (conj (== (cdr _.15) _.2) (== _.16 (closure _.6 _.7)))
                      (eval-expo _.15 ((x)) (_.17 . _.16)))))))))
               (eval-expo _.0 (_.1 . _.2) _.3))))
            (eval-expo _.0 (_.1 . _.2) _.3))
           (disj
            (disj
             (pause
              (state ((lambda (app '(closure (list . _.0) _.1) (lambda _.2)))))
              (conj (== () _.0) (== () (1 x))))
             (pause
              (state ((lambda (app '(closure (list . _.0) _.1) (lambda _.2)))))
              (conj
               (conj
                (conj (== (_.0 . _.1) _.2) (== (_.3 . _.4) (1 x)))
                (eval-expo _.0 ((closure _.5 ((x))) . _.6) _.3))
               (eval-listo _.1 ((closure _.5 ((x))) . _.6) _.4))))
            (disj
             (pause
              (state ((lambda (app '(closure _.0 _.1) (lambda _.2)))))
              (conj
               (== (var _.0) _.1)
               (lookupo _.0 ((closure _.2 ((x))) . _.3) (1 x))))
             (pause
              (state ((lambda (app '(closure _.0 _.1) (lambda _.2)))))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.0 _.1) _.2)
                  (eval-expo
                   _.0
                   ((closure _.3 ((x))) . _.4)
                   (closure _.5 _.6)))
                 (eval-expo _.1 ((closure _.3 ((x))) . _.4) _.7))
                (eval-expo _.5 (_.7 . _.6) (1 x)))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.8 _.9) _.2) (== (_.10 . _.11) (1 x)))
                  (eval-expo _.8 ((closure _.3 ((x))) . _.4) _.10))
                 (eval-expo _.9 ((closure _.3 ((x))) . _.4) _.11))
                (disj
                 (conj
                  (conj (== (car _.12) _.2) (== _.13 (1 x)))
                  (eval-expo _.12 ((closure _.3 ((x))) . _.4) (_.13 . _.14)))
                 (conj
                  (conj (== (cdr _.15) _.2) (== _.16 (1 x)))
                  (eval-expo
                   _.15
                   ((closure _.3 ((x))) . _.4)
                   (_.17 . _.16))))))))))
          (disj
           (disj
            (disj
             (pause
              (state ((lambda (app (lambda _.0) '_.1))))
              (conj
               (conj
                (conj
                 (== (app _.0 _.1) _.2)
                 (eval-expo _.0 (_.3 (x)) (closure _.4 _.5)))
                (eval-expo _.1 (_.3 (x)) _.6))
               (eval-expo _.4 (_.6 . _.5) (1 x))))
             (pause
              (state ((lambda (app (lambda _.0) '_.1))))
              (disj
               (conj
                (conj
                 (conj (== (cons _.0 _.1) _.2) (== (_.3 . _.4) (1 x)))
                 (eval-expo _.0 (_.5 (x)) _.3))
                (eval-expo _.1 (_.5 (x)) _.4))
               (disj
                (conj
                 (conj (== (car _.6) _.2) (== _.7 (1 x)))
                 (eval-expo _.6 (_.5 (x)) (_.7 . _.8)))
                (conj
                 (conj (== (cdr _.9) _.2) (== _.10 (1 x)))
                 (eval-expo _.9 (_.5 (x)) (_.11 . _.10)))))))
            (pause
             (state ((lambda (app (lambda (var _.0)) '_.1))))
             (conj (== (s . _.0) _.1) (lookupo _.0 ((x)) (1 x)))))
           (conj
            (disj
             (pause
              (state ((lambda (app (lambda (list _.0 . _.1)) '_.2))))
              (conj (== '_.0 _.1) (== _.0 1)))
             (pause
              (state ((lambda (app (lambda (list _.0 . _.1)) '_.2))))
              (disj
               (conj (== (list . _.0) _.1) (eval-listo _.0 (_.2 (x)) 1))
               (disj
                (conj (== (var _.3) _.1) (lookupo _.3 (_.2 (x)) 1))
                (disj
                 (conj
                  (conj
                   (conj
                    (== (app _.4 _.5) _.1)
                    (eval-expo _.4 (_.2 (x)) (closure _.6 _.7)))
                   (eval-expo _.5 (_.2 (x)) _.8))
                  (eval-expo _.6 (_.8 . _.7) 1))
                 (disj
                  (conj
                   (conj
                    (conj (== (cons _.9 _.10) _.1) (== (_.11 . _.12) 1))
                    (eval-expo _.9 (_.2 (x)) _.11))
                   (eval-expo _.10 (_.2 (x)) _.12))
                  (disj
                   (conj
                    (conj (== (car _.13) _.1) (== _.14 1))
                    (eval-expo _.13 (_.2 (x)) (_.14 . _.15)))
                   (conj
                    (conj (== (cdr _.16) _.1) (== _.17 1))
                    (eval-expo _.16 (_.2 (x)) (_.18 . _.17))))))))))
            (eval-listo _.0 (_.1 . _.2) _.3)))))))
      (eval-listo _.0 () _.1))
     (conj
      (disj
       (conj
        (disj
         (disj
          (pause
           (state ((lambda (app (lambda (var ())) '(1 x)))))
           (conj (== (lambda _.0) '(y)) (== (closure _.0 ()) _.1)))
          (pause
           (state ((lambda (app (lambda (var ())) '(1 x)))))
           (disj
            (conj (== '_.0 '(y)) (== _.0 _.1))
            (disj
             (conj (== (list . _.2) '(y)) (eval-listo _.2 () _.1))
             (disj
              (conj (== (var _.3) '(y)) (lookupo _.3 () _.1))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.4 _.5) '(y))
                  (eval-expo _.4 () (closure _.6 _.7)))
                 (eval-expo _.5 () _.8))
                (eval-expo _.6 (_.8 . _.7) _.1))
               (disj
                (conj
                 (conj
                  (conj (== (cons _.9 _.10) '(y)) (== (_.11 . _.12) _.1))
                  (eval-expo _.9 () _.11))
                 (eval-expo _.10 () _.12))
                (disj
                 (conj
                  (conj (== (car _.13) '(y)) (== _.14 _.1))
                  (eval-expo _.13 () (_.14 . _.15)))
                 (conj
                  (conj (== (cdr _.16) '(y)) (== _.17 _.1))
                  (eval-expo _.16 () (_.18 . _.17)))))))))))
         (conj
          (disj
           (pause
            (state ((lambda (app (lambda (var ())) '(1 x)))))
            (conj
             (== '_.0 (lambda (app (lambda (var ())) '(1 x))))
             (== _.0 (closure _.1 _.2))))
           (pause
            (state ((lambda (app (lambda (var ())) '(1 x)))))
            (disj
             (conj
              (== (list . _.0) (lambda (app (lambda (var ())) '(1 x))))
              (eval-listo _.0 () (closure _.1 _.2)))
             (disj
              (conj
               (== (var _.3) (lambda (app (lambda (var ())) '(1 x))))
               (lookupo _.3 () (closure _.1 _.2)))
              (disj
               (conj
                (conj
                 (conj
                  (== (app _.4 _.5) (lambda (app (lambda (var ())) '(1 x))))
                  (eval-expo _.4 () (closure _.6 _.7)))
                 (eval-expo _.5 () _.8))
                (eval-expo _.6 (_.8 . _.7) (closure _.1 _.2)))
               (disj
                (conj
                 (conj
                  (conj
                   (== (cons _.9 _.10) (lambda (app (lambda (var ())) '(1 x))))
                   (== (_.11 . _.12) (closure _.1 _.2)))
                  (eval-expo _.9 () _.11))
                 (eval-expo _.10 () _.12))
                (disj
                 (conj
                  (conj
                   (== (car _.13) (lambda (app (lambda (var ())) '(1 x))))
                   (== _.14 (closure _.1 _.2)))
                  (eval-expo _.13 () (_.14 . _.15)))
                 (conj
                  (conj
                   (== (cdr _.16) (lambda (app (lambda (var ())) '(1 x))))
                   (== _.17 (closure _.1 _.2)))
                  (eval-expo _.16 () (_.18 . _.17))))))))))
          (eval-expo _.0 () _.1)))
        (eval-expo _.0 (_.1 . _.2) _.3))
       (disj
        (pause
         (state ((lambda (app (lambda (var ())) '(1 x)))))
         (conj
          (conj
           (== (car _.0) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           (== _.1 (1 y)))
          (eval-expo _.0 () (_.1 . _.2))))
        (pause
         (state ((lambda (app (lambda (var ())) '(1 x)))))
         (conj
          (conj
           (== (cdr _.0) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           (== _.1 (1 y)))
          (eval-expo _.0 () (_.2 . _.1))))))
      (eval-listo _.0 () _.1)))
    (conj
     (disj
      (conj
       (disj
        (conj
         (disj
          (pause
           (state ((lambda (list (car '(1 . _.0)) 'x))))
           (conj
            (conj
             (conj
              (== (app _.0 _.1) (lambda (list (car '(1 . _.2)) 'x)))
              (eval-expo _.0 () (closure _.3 _.4)))
             (eval-expo _.1 () _.5))
            (eval-expo _.3 (_.5 . _.4) (closure _.6 _.7))))
          (pause
           (state ((lambda (list (car '(1 . _.0)) 'x))))
           (disj
            (conj
             (conj
              (conj
               (== (cons _.0 _.1) (lambda (list (car '(1 . _.2)) 'x)))
               (== (_.3 . _.4) (closure _.5 _.6)))
              (eval-expo _.0 () _.3))
             (eval-expo _.1 () _.4))
            (disj
             (conj
              (conj
               (== (car _.7) (lambda (list (car '(1 . _.2)) 'x)))
               (== _.8 (closure _.5 _.6)))
              (eval-expo _.7 () (_.8 . _.9)))
             (conj
              (conj
               (== (cdr _.10) (lambda (list (car '(1 . _.2)) 'x)))
               (== _.11 (closure _.5 _.6)))
              (eval-expo _.10 () (_.12 . _.11)))))))
         (eval-expo _.0 () _.1))
        (pause
         (state ((lambda (list (car '(1 . _.0)) 'x))))
         (disj
          (conj
           (conj
            (conj (== (app _.0 _.1) '(y)) (eval-expo _.0 () (closure _.2 _.3)))
            (eval-expo _.1 () _.4))
           (eval-expo _.2 (_.4 . _.3) _.5))
          (disj
           (conj
            (conj
             (conj (== (cons _.6 _.7) '(y)) (== (_.8 . _.9) _.5))
             (eval-expo _.6 () _.8))
            (eval-expo _.7 () _.9))
           (disj
            (conj
             (conj (== (car _.10) '(y)) (== _.11 _.5))
             (eval-expo _.10 () (_.11 . _.12)))
            (conj
             (conj (== (cdr _.13) '(y)) (== _.14 _.5))
             (eval-expo _.13 () (_.15 . _.14))))))))
       (eval-expo _.0 (_.1 . _.2) _.3))
      (disj
       (pause
        (state ((lambda (list (car '(1 . _.0)) 'x))))
        (conj
         (conj
          (conj (== (_.0 . _.1) ((car '(1 . _.2)) 'x)) (== (_.3 . _.4) (1 y)))
          (eval-expo _.0 ((y)) _.3))
         (eval-listo _.1 ((y)) _.4)))
       (pause
        (state ((lambda (list (car '(1 . _.0)) 'x))))
        (disj
         (conj
          (conj
           (conj
            (== (app _.0 _.1) (list (car '(1 . _.2)) 'x))
            (eval-expo _.0 ((y)) (closure _.3 _.4)))
           (eval-expo _.1 ((y)) _.5))
          (eval-expo _.3 (_.5 . _.4) (1 y)))
         (disj
          (conj
           (conj
            (conj
             (== (cons _.6 _.7) (list (car '(1 . _.2)) 'x))
             (== (_.8 . _.9) (1 y)))
            (eval-expo _.6 ((y)) _.8))
           (eval-expo _.7 ((y)) _.9))
          (disj
           (conj
            (conj (== (car _.10) (list (car '(1 . _.2)) 'x)) (== _.11 (1 y)))
            (eval-expo _.10 ((y)) (_.11 . _.12)))
           (conj
            (conj (== (cdr _.13) (list (car '(1 . _.2)) 'x)) (== _.14 (1 y)))
            (eval-expo _.13 ((y)) (_.15 . _.14)))))))))
     (eval-listo _.0 () _.1)))
   (disj
    (conj
     (disj
      (disj
       (disj
        (pause
         (state ((lambda (cons '1 (var ())))))
         (conj
          (conj
           (conj
            (== (app _.0 _.1) (var ()))
            (eval-expo _.0 ((y)) (closure _.2 _.3)))
           (eval-expo _.1 ((y)) _.4))
          (eval-expo _.2 (_.4 . _.3) (y))))
        (pause
         (state ((lambda (cons '1 (var ())))))
         (disj
          (conj
           (conj
            (conj (== (cons _.0 _.1) (var ())) (== (_.2 . _.3) (y)))
            (eval-expo _.0 ((y)) _.2))
           (eval-expo _.1 ((y)) _.3))
          (disj
           (conj
            (conj (== (car _.4) (var ())) (== _.5 (y)))
            (eval-expo _.4 ((y)) (_.5 . _.6)))
           (conj
            (conj (== (cdr _.7) (var ())) (== _.8 (y)))
            (eval-expo _.7 ((y)) (_.9 . _.8)))))))
       (pause
        (state ((lambda (cons '1 (var ())))))
        (conj (== (s . _.0) ()) (lookupo _.0 () (y)))))
      (conj
       (pause
        (state ((lambda (cons '1 (var ())))))
        (conj
         (conj (== (cdr _.0) '1) (== _.1 1))
         (eval-expo _.0 ((y)) (_.2 . _.1))))
       (eval-expo _.0 (_.1 . _.2) _.3)))
     (eval-listo _.0 () _.1))
    (pause
     (state ((lambda (cons '1 (var ())))))
     (conj
      (conj
       (conj (== (_.0 . _.1) ()) (== (_.2 . _.3) ()))
       (eval-expo _.0 () _.2))
      (eval-listo _.1 () _.3))))))
