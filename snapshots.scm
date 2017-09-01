(load "transparent-evalo.scm")

(define (q-np n)
  (query (defn)
         (fresh (body)
           (== `(lambda ,body) defn)
           (evalo `(list (app ,defn '(x)) (app ,defn '(y)))
                  `((,n x) (,n y))))))

(define q-quine (query (p) (evalo p p)))

(define (print-labeled-solution q)
  (let loop ((choices (labeled-pretty (labeled-solution q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (if (caar choices) 0 1) (cadar choices))
      (loop (cdr choices)))))

(define (print-labeled-solution* q)
  (define (boolean->idx b) (if b 0 1))
  (let loop ((choices (labeled-pretty (labeled-solution* q))))
    (when (pair? choices)
      (printf "(~s ~s)\n" (map boolean->idx (caar choices)) (cadar choices))
      (loop (cdr choices)))))

(print-labeled-solution* (q-np 1))

;(print-labeled-solution q-quine)

;; (stream-pretty q-1p)
;; Initial snapshot

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(conj
     ;(== (lambda _.3) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
     ;(== (closure _.3 ()) ((1 x) (1 y)))))
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(== '_.4 (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(== _.4 ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(== (list . _.5) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
       ;(eval-listo _.5 () ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
        ;(lookupo _.6 () ((1 x) (1 y))))
       ;(disj
        ;(conj
         ;(conj
          ;(conj
           ;(==
            ;(app _.7 _.8)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(eval-expo _.7 () (closure _.11 _.10)))
          ;(eval-expo _.8 () _.9))
         ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
        ;(disj
         ;(conj
          ;(conj
           ;(conj
            ;(==
             ;(cons _.12 _.13)
             ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
            ;(== (_.14 . _.15) ((1 x) (1 y))))
           ;(eval-expo _.12 () _.14))
          ;(eval-expo _.13 () _.15))
         ;(disj
          ;(conj
           ;(conj
            ;(==
             ;(car _.16)
             ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
            ;(== _.17 ((1 x) (1 y))))
           ;(eval-expo _.16 () (_.17 . _.18)))
          ;(conj
           ;(conj
            ;(==
             ;(cdr _.19)
             ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
            ;(== _.21 ((1 x) (1 y))))
           ;(eval-expo _.19 () (_.20 . _.21))))))))))))


;;; (stream-pretty (step 1 q-1p))
;;; Snapshot 1 step in

;'(()
  ;(pause
   ;(state ((lambda _.2)))
   ;(disj
    ;(conj
     ;(== '_.4 (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
     ;(== _.4 ((1 x) (1 y))))
    ;(disj
     ;(conj
      ;(== (list . _.5) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(eval-listo _.5 () ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
       ;(lookupo _.6 () ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(app _.7 _.8)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(eval-expo _.7 () (closure _.11 _.10)))
         ;(eval-expo _.8 () _.9))
        ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
       ;(disj
        ;(conj
         ;(conj
          ;(conj
           ;(==
            ;(cons _.12 _.13)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== (_.14 . _.15) ((1 x) (1 y))))
          ;(eval-expo _.12 () _.14))
         ;(eval-expo _.13 () _.15))
        ;(disj
         ;(conj
          ;(conj
           ;(==
            ;(car _.16)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== _.17 ((1 x) (1 y))))
          ;(eval-expo _.16 () (_.17 . _.18)))
         ;(conj
          ;(conj
           ;(==
            ;(cdr _.19)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== _.21 ((1 x) (1 y))))
          ;(eval-expo _.19 () (_.20 . _.21)))))))))))


;;; (stream-pretty (step 2 q-1p))

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(conj
     ;(== '_.4 (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
     ;(== _.4 ((1 x) (1 y)))))
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(== (list . _.5) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(eval-listo _.5 () ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
       ;(lookupo _.6 () ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(app _.7 _.8)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(eval-expo _.7 () (closure _.11 _.10)))
         ;(eval-expo _.8 () _.9))
        ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
       ;(disj
        ;(conj
         ;(conj
          ;(conj
           ;(==
            ;(cons _.12 _.13)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== (_.14 . _.15) ((1 x) (1 y))))
          ;(eval-expo _.12 () _.14))
         ;(eval-expo _.13 () _.15))
        ;(disj
         ;(conj
          ;(conj
           ;(==
            ;(car _.16)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== _.17 ((1 x) (1 y))))
          ;(eval-expo _.16 () (_.17 . _.18)))
         ;(conj
          ;(conj
           ;(==
            ;(cdr _.19)
            ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
           ;(== _.21 ((1 x) (1 y))))
          ;(eval-expo _.19 () (_.20 . _.21)))))))))))


;;; (stream-pretty (step 3 q-1p))

;'(()
  ;(pause
   ;(state ((lambda _.2)))
   ;(disj
    ;(conj
     ;(== (list . _.5) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
     ;(eval-listo _.5 () ((1 x) (1 y))))
    ;(disj
     ;(conj
      ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(lookupo _.6 () ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(app _.7 _.8)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(eval-expo _.7 () (closure _.11 _.10)))
        ;(eval-expo _.8 () _.9))
       ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(cons _.12 _.13)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== (_.14 . _.15) ((1 x) (1 y))))
         ;(eval-expo _.12 () _.14))
        ;(eval-expo _.13 () _.15))
       ;(disj
        ;(conj
         ;(conj
          ;(==
           ;(car _.16)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.17 ((1 x) (1 y))))
         ;(eval-expo _.16 () (_.17 . _.18)))
        ;(conj
         ;(conj
          ;(==
           ;(cdr _.19)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.21 ((1 x) (1 y))))
         ;(eval-expo _.19 () (_.20 . _.21))))))))))


;;; (stream-pretty (step 4 q-1p))

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(conj
     ;(== (list . _.5) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
     ;(eval-listo _.5 () ((1 x) (1 y)))))
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(lookupo _.6 () ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(app _.7 _.8)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(eval-expo _.7 () (closure _.11 _.10)))
        ;(eval-expo _.8 () _.9))
       ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(cons _.12 _.13)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== (_.14 . _.15) ((1 x) (1 y))))
         ;(eval-expo _.12 () _.14))
        ;(eval-expo _.13 () _.15))
       ;(disj
        ;(conj
         ;(conj
          ;(==
           ;(car _.16)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.17 ((1 x) (1 y))))
         ;(eval-expo _.16 () (_.17 . _.18)))
        ;(conj
         ;(conj
          ;(==
           ;(cdr _.19)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.21 ((1 x) (1 y))))
         ;(eval-expo _.19 () (_.20 . _.21))))))))))


;;; (stream-pretty (step 5 q-1p))

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(lookupo _.6 () ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(app _.7 _.8)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(eval-expo _.7 () (closure _.11 _.10)))
        ;(eval-expo _.8 () _.9))
       ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(cons _.12 _.13)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== (_.14 . _.15) ((1 x) (1 y))))
         ;(eval-expo _.12 () _.14))
        ;(eval-expo _.13 () _.15))
       ;(disj
        ;(conj
         ;(conj
          ;(==
           ;(car _.16)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.17 ((1 x) (1 y))))
         ;(eval-expo _.16 () (_.17 . _.18)))
        ;(conj
         ;(conj
          ;(==
           ;(cdr _.19)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.21 ((1 x) (1 y))))
         ;(eval-expo _.19 () (_.20 . _.21))))))))
   ;(disj
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(== () ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(== () ((1 x) (1 y)))))
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(conj
       ;(conj
        ;(== (_.22 . _.23) ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
        ;(== (_.24 . _.25) ((1 x) (1 y))))
       ;(eval-expo _.22 () _.24))
      ;(eval-listo _.23 () _.25))))))


;;; (stream-pretty (step 6 q-1p))

;'(()
  ;(disj
   ;(disj
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(== () ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(== () ((1 x) (1 y)))))
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(conj
       ;(conj
        ;(== (_.26 . _.27) ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
        ;(== (_.28 . _.29) ((1 x) (1 y))))
       ;(eval-expo _.26 () _.28))
      ;(eval-listo _.27 () _.29))))
   ;(disj
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(lookupo _.6 () ((1 x) (1 y)))))
    ;(pause
     ;(state ((lambda _.2)))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(app _.7 _.8)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(eval-expo _.7 () (closure _.11 _.10)))
        ;(eval-expo _.8 () _.9))
       ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(cons _.12 _.13)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== (_.14 . _.15) ((1 x) (1 y))))
         ;(eval-expo _.12 () _.14))
        ;(eval-expo _.13 () _.15))
       ;(disj
        ;(conj
         ;(conj
          ;(==
           ;(car _.16)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.17 ((1 x) (1 y))))
         ;(eval-expo _.16 () (_.17 . _.18)))
        ;(conj
         ;(conj
          ;(==
           ;(cdr _.19)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.21 ((1 x) (1 y))))
         ;(eval-expo _.19 () (_.20 . _.21))))))))))


;;; (stream-pretty (step 7 q-1p))

;'(()
  ;(disj
   ;(disj
    ;(pause
     ;(state ((lambda _.2)))
     ;(conj
      ;(== (var _.6) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
      ;(lookupo _.6 () ((1 x) (1 y)))))
    ;(pause
     ;(state ((lambda _.2)))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(app _.7 _.8)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(eval-expo _.7 () (closure _.11 _.10)))
        ;(eval-expo _.8 () _.9))
       ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
      ;(disj
       ;(conj
        ;(conj
         ;(conj
          ;(==
           ;(cons _.12 _.13)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== (_.14 . _.15) ((1 x) (1 y))))
         ;(eval-expo _.12 () _.14))
        ;(eval-expo _.13 () _.15))
       ;(disj
        ;(conj
         ;(conj
          ;(==
           ;(car _.16)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.17 ((1 x) (1 y))))
         ;(eval-expo _.16 () (_.17 . _.18)))
        ;(conj
         ;(conj
          ;(==
           ;(cdr _.19)
           ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
          ;(== _.21 ((1 x) (1 y))))
         ;(eval-expo _.19 () (_.20 . _.21))))))))
   ;(pause
    ;(state ((lambda _.2)))
    ;(conj
     ;(conj
      ;(conj
       ;(== (_.30 . _.31) ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
       ;(== (_.32 . _.33) ((1 x) (1 y))))
      ;(eval-expo _.30 () _.32))
     ;(eval-listo _.31 () _.33)))))


;;; (stream-pretty (step 8 q-1p))

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(conj
     ;(conj
      ;(conj
       ;(== (_.34 . _.35) ((app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
       ;(== (_.36 . _.37) ((1 x) (1 y))))
      ;(eval-expo _.34 () _.36))
     ;(eval-listo _.35 () _.37)))
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(conj
       ;(conj
        ;(==
         ;(app _.7 _.8)
         ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
        ;(eval-expo _.7 () (closure _.11 _.10)))
       ;(eval-expo _.8 () _.9))
      ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(cons _.12 _.13)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== (_.14 . _.15) ((1 x) (1 y))))
        ;(eval-expo _.12 () _.14))
       ;(eval-expo _.13 () _.15))
      ;(disj
       ;(conj
        ;(conj
         ;(== (car _.16) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== _.17 ((1 x) (1 y))))
        ;(eval-expo _.16 () (_.17 . _.18)))
       ;(conj
        ;(conj
         ;(== (cdr _.19) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== _.21 ((1 x) (1 y))))
        ;(eval-expo _.19 () (_.20 . _.21)))))))))


;;; (stream-pretty (step 9 q-1p))

;'(()
  ;(disj
   ;(pause
    ;(state ((lambda _.2)))
    ;(disj
     ;(conj
      ;(conj
       ;(conj
        ;(==
         ;(app _.7 _.8)
         ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
        ;(eval-expo _.7 () (closure _.11 _.10)))
       ;(eval-expo _.8 () _.9))
      ;(eval-expo _.11 (_.9 . _.10) ((1 x) (1 y))))
     ;(disj
      ;(conj
       ;(conj
        ;(conj
         ;(==
          ;(cons _.12 _.13)
          ;(list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== (_.14 . _.15) ((1 x) (1 y))))
        ;(eval-expo _.12 () _.14))
       ;(eval-expo _.13 () _.15))
      ;(disj
       ;(conj
        ;(conj
         ;(== (car _.16) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== _.17 ((1 x) (1 y))))
        ;(eval-expo _.16 () (_.17 . _.18)))
       ;(conj
        ;(conj
         ;(== (cdr _.19) (list (app (lambda _.2) '(x)) (app (lambda _.2) '(y))))
         ;(== _.21 ((1 x) (1 y))))
        ;(eval-expo _.19 () (_.20 . _.21)))))))
   ;(conj
    ;(disj
     ;(pause
      ;(state ((lambda _.2)))
      ;(conj
       ;(== (lambda _.42) (app (lambda _.2) '(x)))
       ;(== (closure _.42 ()) (1 x))))
     ;(pause
      ;(state ((lambda _.2)))
      ;(disj
       ;(conj (== '_.43 (app (lambda _.2) '(x))) (== _.43 (1 x)))
       ;(disj
        ;(conj
         ;(== (list . _.44) (app (lambda _.2) '(x)))
         ;(eval-listo _.44 () (1 x)))
        ;(disj
         ;(conj (== (var _.45) (app (lambda _.2) '(x))) (lookupo _.45 () (1 x)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (app _.46 _.47) (app (lambda _.2) '(x)))
             ;(eval-expo _.46 () (closure _.50 _.49)))
            ;(eval-expo _.47 () _.48))
           ;(eval-expo _.50 (_.48 . _.49) (1 x)))
          ;(disj
           ;(conj
            ;(conj
             ;(conj
              ;(== (cons _.51 _.52) (app (lambda _.2) '(x)))
              ;(== (_.53 . _.54) (1 x)))
             ;(eval-expo _.51 () _.53))
            ;(eval-expo _.52 () _.54))
           ;(disj
            ;(conj
             ;(conj (== (car _.55) (app (lambda _.2) '(x))) (== _.56 (1 x)))
             ;(eval-expo _.55 () (_.56 . _.57)))
            ;(conj
             ;(conj (== (cdr _.58) (app (lambda _.2) '(x))) (== _.60 (1 x)))
             ;(eval-expo _.58 () (_.59 . _.60)))))))))))
    ;(eval-listo _.39 () _.41))))


;;; (stream-pretty (step 50 q-1p))
;;; Snapshot 50 steps in

;'(()
  ;(disj
   ;(conj
    ;(pause
     ;(state ((lambda '(1 x))))
     ;(disj
      ;(conj (== '_.107 (app (lambda '(1 x)) '(y))) (== _.107 (1 y)))
      ;(disj
       ;(conj
        ;(== (list . _.108) (app (lambda '(1 x)) '(y)))
        ;(eval-listo _.108 () (1 y)))
       ;(disj
        ;(conj
         ;(== (var _.109) (app (lambda '(1 x)) '(y)))
         ;(lookupo _.109 () (1 y)))
        ;(disj
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.110 _.111) (app (lambda '(1 x)) '(y)))
            ;(eval-expo _.110 () (closure _.114 _.113)))
           ;(eval-expo _.111 () _.112))
          ;(eval-expo _.114 (_.112 . _.113) (1 y)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (cons _.115 _.116) (app (lambda '(1 x)) '(y)))
             ;(== (_.117 . _.118) (1 y)))
            ;(eval-expo _.115 () _.117))
           ;(eval-expo _.116 () _.118))
          ;(disj
           ;(conj
            ;(conj (== (car _.119) (app (lambda '(1 x)) '(y))) (== _.120 (1 y)))
            ;(eval-expo _.119 () (_.120 . _.121)))
           ;(conj
            ;(conj (== (cdr _.122) (app (lambda '(1 x)) '(y))) (== _.124 (1 y)))
            ;(eval-expo _.122 () (_.123 . _.124))))))))))
    ;(eval-listo _.103 () _.105))
   ;(conj
    ;(disj
     ;(conj
      ;(disj
       ;(pause
        ;(state ((lambda _.2)))
        ;(disj
         ;(conj (== (var _.67) '(x)) (lookupo _.67 () _.32))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (app _.68 _.69) '(x))
             ;(eval-expo _.68 () (closure _.72 _.71)))
            ;(eval-expo _.69 () _.70))
           ;(eval-expo _.72 (_.70 . _.71) _.32))
          ;(disj
           ;(conj
            ;(conj
             ;(conj (== (cons _.73 _.74) '(x)) (== (_.75 . _.76) _.32))
             ;(eval-expo _.73 () _.75))
            ;(eval-expo _.74 () _.76))
           ;(disj
            ;(conj
             ;(conj (== (car _.77) '(x)) (== _.78 _.32))
             ;(eval-expo _.77 () (_.78 . _.79)))
            ;(conj
             ;(conj (== (cdr _.80) '(x)) (== _.82 _.32))
             ;(eval-expo _.80 () (_.81 . _.82))))))))
       ;(conj
        ;(pause
         ;(state ((lambda _.2)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (app _.49 _.50) (lambda _.2))
             ;(eval-expo _.49 () (closure _.53 _.52)))
            ;(eval-expo _.50 () _.51))
           ;(eval-expo _.53 (_.51 . _.52) (closure _.34 _.33)))
          ;(disj
           ;(conj
            ;(conj
             ;(conj
              ;(== (cons _.54 _.55) (lambda _.2))
              ;(== (_.56 . _.57) (closure _.34 _.33)))
             ;(eval-expo _.54 () _.56))
            ;(eval-expo _.55 () _.57))
           ;(disj
            ;(conj
             ;(conj (== (car _.58) (lambda _.2)) (== _.59 (closure _.34 _.33)))
             ;(eval-expo _.58 () (_.59 . _.60)))
            ;(conj
             ;(conj (== (cdr _.61) (lambda _.2)) (== _.63 (closure _.34 _.33)))
             ;(eval-expo _.61 () (_.62 . _.63)))))))
        ;(eval-expo _.31 () _.32)))
      ;(eval-expo _.34 (_.32 . _.33) _.24))
     ;(disj
      ;(pause
       ;(state ((lambda _.2)))
       ;(disj
        ;(conj (== (var _.86) _.2) (lookupo _.86 ((x)) (1 x)))
        ;(disj
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.87 _.88) _.2)
            ;(eval-expo _.87 ((x)) (closure _.91 _.90)))
           ;(eval-expo _.88 ((x)) _.89))
          ;(eval-expo _.91 (_.89 . _.90) (1 x)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj (== (cons _.92 _.93) _.2) (== (_.94 . _.95) (1 x)))
            ;(eval-expo _.92 ((x)) _.94))
           ;(eval-expo _.93 ((x)) _.95))
          ;(disj
           ;(conj
            ;(conj (== (car _.96) _.2) (== _.97 (1 x)))
            ;(eval-expo _.96 ((x)) (_.97 . _.98)))
           ;(conj
            ;(conj (== (cdr _.99) _.2) (== _.101 (1 x)))
            ;(eval-expo _.99 ((x)) (_.100 . _.101))))))))
      ;(disj
       ;(pause
        ;(state ((lambda (list . _.85))))
        ;(conj (== () _.85) (== () (1 x))))
       ;(pause
        ;(state ((lambda (list . _.85))))
        ;(conj
         ;(conj
          ;(conj (== (_.125 . _.126) _.85) (== (_.127 . _.128) (1 x)))
          ;(eval-expo _.125 ((x)) _.127))
         ;(eval-listo _.126 ((x)) _.128))))))
    ;(eval-listo _.23 () _.25))))


;;; (stream-pretty (step 1607 q-1p))
;;; Snapshot right before the first answer appears

;'(()
  ;(disj
   ;(disj
    ;(disj
     ;(pause (state ((lambda (cons '1 (var ()))))) (conj (== () ()) (== () ())))
     ;(pause
      ;(state ((lambda (cons '1 (var ())))))
      ;(conj
       ;(conj
        ;(conj (== (_.3077 . _.3078) ()) (== (_.3079 . _.3080) ()))
        ;(eval-expo _.3077 () _.3079))
       ;(eval-listo _.3078 () _.3080))))
    ;(conj
     ;(disj
      ;(disj
       ;(disj
        ;(pause
         ;(state ((lambda (cons '1 (var ())))))
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.3008 _.3009) (var ()))
            ;(eval-expo _.3008 ((y)) (closure _.3012 _.3011)))
           ;(eval-expo _.3009 ((y)) _.3010))
          ;(eval-expo _.3012 (_.3010 . _.3011) (y))))
        ;(pause
         ;(state ((lambda (cons '1 (var ())))))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (cons _.3013 _.3014) (var ()))
             ;(== (_.3015 . _.3016) (y)))
            ;(eval-expo _.3013 ((y)) _.3015))
           ;(eval-expo _.3014 ((y)) _.3016))
          ;(disj
           ;(conj
            ;(conj (== (car _.3017) (var ())) (== _.3018 (y)))
            ;(eval-expo _.3017 ((y)) (_.3018 . _.3019)))
           ;(conj
            ;(conj (== (cdr _.3020) (var ())) (== _.3022 (y)))
            ;(eval-expo _.3020 ((y)) (_.3021 . _.3022)))))))
       ;(pause
        ;(state ((lambda (cons '1 (var ())))))
        ;(conj (== (s . _.3076) ()) (lookupo _.3076 () (y)))))
      ;(conj
       ;(pause
        ;(state ((lambda (cons '1 (var ())))))
        ;(conj
         ;(conj (== (cdr _.2959) '1) (== _.2961 1))
         ;(eval-expo _.2959 ((y)) (_.2960 . _.2961))))
       ;(eval-expo _.2858 (_.2728 . _.2729) _.2860)))
     ;(eval-listo _.2719 () _.2721)))
   ;(disj
    ;(disj
     ;(conj
      ;(disj
       ;(disj
        ;(disj
         ;(disj
          ;(disj
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state ((lambda (list (cdr _.274) . _.233))))
               ;(disj
                ;(conj
                 ;(== (list . _.927) _.274)
                 ;(eval-listo _.927 ((x)) (_.275 . 1)))
                ;(disj
                 ;(conj
                  ;(== (var _.928) _.274)
                  ;(lookupo _.928 ((x)) (_.275 . 1)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.929 _.930) _.274)
                     ;(eval-expo _.929 ((x)) (closure _.933 _.932)))
                    ;(eval-expo _.930 ((x)) _.931))
                   ;(eval-expo _.933 (_.931 . _.932) (_.275 . 1)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.934 _.935) _.274)
                      ;(== (_.936 . _.937) (_.275 . 1)))
                     ;(eval-expo _.934 ((x)) _.936))
                    ;(eval-expo _.935 ((x)) _.937))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.938) _.274) (== _.939 (_.275 . 1)))
                     ;(eval-expo _.938 ((x)) (_.939 . _.940)))
                    ;(conj
                     ;(conj (== (cdr _.941) _.274) (== _.943 (_.275 . 1)))
                     ;(eval-expo _.941 ((x)) (_.942 . _.943)))))))))
              ;(disj
               ;(pause
                ;(state ((lambda (list (car _.271) . _.233))))
                ;(conj
                 ;(== (list . _.825) _.271)
                 ;(eval-listo _.825 ((x)) (1 . _.273))))
               ;(pause
                ;(state ((lambda (list (car _.271) . _.233))))
                ;(disj
                 ;(conj
                  ;(== (var _.826) _.271)
                  ;(lookupo _.826 ((x)) (1 . _.273)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.827 _.828) _.271)
                     ;(eval-expo _.827 ((x)) (closure _.831 _.830)))
                    ;(eval-expo _.828 ((x)) _.829))
                   ;(eval-expo _.831 (_.829 . _.830) (1 . _.273)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.832 _.833) _.271)
                      ;(== (_.834 . _.835) (1 . _.273)))
                     ;(eval-expo _.832 ((x)) _.834))
                    ;(eval-expo _.833 ((x)) _.835))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.836) _.271) (== _.837 (1 . _.273)))
                     ;(eval-expo _.836 ((x)) (_.837 . _.838)))
                    ;(conj
                     ;(conj (== (cdr _.839) _.271) (== _.841 (1 . _.273)))
                     ;(eval-expo _.839 ((x)) (_.840 . _.841))))))))))
             ;(disj
              ;(disj
               ;(pause
                ;(state
                 ;((lambda (list (app (lambda _.266) (lambda _.652)) . _.233))))
                ;(conj
                 ;(== (list . _.787) _.266)
                 ;(eval-listo _.787 ((closure _.652 ((x))) (x)) 1)))
               ;(pause
                ;(state
                 ;((lambda (list (app (lambda _.266) (lambda _.652)) . _.233))))
                ;(disj
                 ;(conj
                  ;(== (var _.788) _.266)
                  ;(lookupo _.788 ((closure _.652 ((x))) (x)) 1))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.789 _.790) _.266)
                     ;(eval-expo
                      ;_.789
                      ;((closure _.652 ((x))) (x))
                      ;(closure _.793 _.792)))
                    ;(eval-expo _.790 ((closure _.652 ((x))) (x)) _.791))
                   ;(eval-expo _.793 (_.791 . _.792) 1))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.794 _.795) _.266)
                      ;(== (_.796 . _.797) 1))
                     ;(eval-expo _.794 ((closure _.652 ((x))) (x)) _.796))
                    ;(eval-expo _.795 ((closure _.652 ((x))) (x)) _.797))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.798) _.266) (== _.799 1))
                     ;(eval-expo
                      ;_.798
                      ;((closure _.652 ((x))) (x))
                      ;(_.799 . _.800)))
                    ;(conj
                     ;(conj (== (cdr _.801) _.266) (== _.803 1))
                     ;(eval-expo
                      ;_.801
                      ;((closure _.652 ((x))) (x))
                      ;(_.802 . _.803)))))))))
              ;(disj
               ;(disj
                ;(pause
                 ;(state ((lambda (list (app (lambda _.266) '_.264) . _.233))))
                 ;(conj
                  ;(== (lambda _.1503) _.266)
                  ;(== (closure _.1503 (_.264 (x))) 1)))
                ;(pause
                 ;(state ((lambda (list (app (lambda _.266) '_.264) . _.233))))
                 ;(disj
                  ;(conj (== '_.1504 _.266) (== _.1504 1))
                  ;(disj
                   ;(conj
                    ;(== (list . _.1505) _.266)
                    ;(eval-listo _.1505 (_.264 (x)) 1))
                   ;(disj
                    ;(conj
                     ;(== (var _.1506) _.266)
                     ;(lookupo _.1506 (_.264 (x)) 1))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.1507 _.1508) _.266)
                        ;(eval-expo _.1507 (_.264 (x)) (closure _.1511 _.1510)))
                       ;(eval-expo _.1508 (_.264 (x)) _.1509))
                      ;(eval-expo _.1511 (_.1509 . _.1510) 1))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.1512 _.1513) _.266)
                         ;(== (_.1514 . _.1515) 1))
                        ;(eval-expo _.1512 (_.264 (x)) _.1514))
                       ;(eval-expo _.1513 (_.264 (x)) _.1515))
                      ;(disj
                       ;(conj
                        ;(conj (== (car _.1516) _.266) (== _.1517 1))
                        ;(eval-expo _.1516 (_.264 (x)) (_.1517 . _.1518)))
                       ;(conj
                        ;(conj (== (cdr _.1519) _.266) (== _.1521 1))
                        ;(eval-expo
                         ;_.1519
                         ;(_.264 (x))
                         ;(_.1520 . _.1521)))))))))))
               ;(disj
                ;(conj
                 ;(disj
                  ;(pause
                   ;(state ((lambda (list (app (lambda _.266) _.263) . _.233))))
                   ;(disj
                    ;(conj
                     ;(== (list . _.654) _.263)
                     ;(eval-listo _.654 ((x)) _.264))
                    ;(disj
                     ;(conj (== (var _.655) _.263) (lookupo _.655 ((x)) _.264))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (app _.656 _.657) _.263)
                         ;(eval-expo _.656 ((x)) (closure _.660 _.659)))
                        ;(eval-expo _.657 ((x)) _.658))
                       ;(eval-expo _.660 (_.658 . _.659) _.264))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(conj
                          ;(== (cons _.661 _.662) _.263)
                          ;(== (_.663 . _.664) _.264))
                         ;(eval-expo _.661 ((x)) _.663))
                        ;(eval-expo _.662 ((x)) _.664))
                       ;(disj
                        ;(conj
                         ;(conj (== (car _.665) _.263) (== _.666 _.264))
                         ;(eval-expo _.665 ((x)) (_.666 . _.667)))
                        ;(conj
                         ;(conj (== (cdr _.668) _.263) (== _.670 _.264))
                         ;(eval-expo _.668 ((x)) (_.669 . _.670)))))))))
                  ;(disj
                   ;(conj
                    ;(disj
                     ;(pause
                      ;(state ((lambda (list (app _.262 _.263) . _.233))))
                      ;(conj
                       ;(== (list . _.533) _.262)
                       ;(eval-listo _.533 ((x)) (closure _.266 _.265))))
                     ;(pause
                      ;(state ((lambda (list (app _.262 _.263) . _.233))))
                      ;(disj
                       ;(conj
                        ;(== (var _.534) _.262)
                        ;(lookupo _.534 ((x)) (closure _.266 _.265)))
                       ;(disj
                        ;(conj
                         ;(conj
                          ;(conj
                           ;(== (app _.535 _.536) _.262)
                           ;(eval-expo _.535 ((x)) (closure _.539 _.538)))
                          ;(eval-expo _.536 ((x)) _.537))
                         ;(eval-expo
                          ;_.539
                          ;(_.537 . _.538)
                          ;(closure _.266 _.265)))
                        ;(disj
                         ;(conj
                          ;(conj
                           ;(conj
                            ;(== (cons _.540 _.541) _.262)
                            ;(== (_.542 . _.543) (closure _.266 _.265)))
                           ;(eval-expo _.540 ((x)) _.542))
                          ;(eval-expo _.541 ((x)) _.543))
                         ;(disj
                          ;(conj
                           ;(conj
                            ;(== (car _.544) _.262)
                            ;(== _.545 (closure _.266 _.265)))
                           ;(eval-expo _.544 ((x)) (_.545 . _.546)))
                          ;(conj
                           ;(conj
                            ;(== (cdr _.547) _.262)
                            ;(== _.549 (closure _.266 _.265)))
                           ;(eval-expo _.547 ((x)) (_.548 . _.549)))))))))
                    ;(eval-expo _.263 (_.139 . _.140) _.264))
                   ;(pause
                    ;(state
                     ;((lambda (list
                               ;(app '(closure _.266 _.265) _.263)
                               ;.
                               ;_.233))))
                    ;(disj
                     ;(conj (== '_.888 _.263) (== _.888 _.264))
                     ;(disj
                      ;(conj
                       ;(== (list . _.889) _.263)
                       ;(eval-listo _.889 ((x)) _.264))
                      ;(disj
                       ;(conj
                        ;(== (var _.890) _.263)
                        ;(lookupo _.890 ((x)) _.264))
                       ;(disj
                        ;(conj
                         ;(conj
                          ;(conj
                           ;(== (app _.891 _.892) _.263)
                           ;(eval-expo _.891 ((x)) (closure _.895 _.894)))
                          ;(eval-expo _.892 ((x)) _.893))
                         ;(eval-expo _.895 (_.893 . _.894) _.264))
                        ;(disj
                         ;(conj
                          ;(conj
                           ;(conj
                            ;(== (cons _.896 _.897) _.263)
                            ;(== (_.898 . _.899) _.264))
                           ;(eval-expo _.896 ((x)) _.898))
                          ;(eval-expo _.897 ((x)) _.899))
                         ;(disj
                          ;(conj
                           ;(conj (== (car _.900) _.263) (== _.901 _.264))
                           ;(eval-expo _.900 ((x)) (_.901 . _.902)))
                          ;(conj
                           ;(conj (== (cdr _.903) _.263) (== _.905 _.264))
                           ;(eval-expo _.903 ((x)) (_.904 . _.905))))))))))))
                 ;(eval-expo _.266 (_.264 . _.265) _.234))
                ;(disj
                 ;(pause
                  ;(state
                   ;((lambda (list
                             ;(app '(closure _.266 _.265) (lambda _.887))
                             ;.
                             ;_.233))))
                  ;(conj
                   ;(== (lambda _.2680) _.266)
                   ;(== (closure _.2680 ((closure _.887 ((x))) . _.265)) 1)))
                 ;(pause
                  ;(state
                   ;((lambda (list
                             ;(app '(closure _.266 _.265) (lambda _.887))
                             ;.
                             ;_.233))))
                  ;(disj
                   ;(conj (== '_.2681 _.266) (== _.2681 1))
                   ;(disj
                    ;(conj
                     ;(== (list . _.2682) _.266)
                     ;(eval-listo _.2682 ((closure _.887 ((x))) . _.265) 1))
                    ;(disj
                     ;(conj
                      ;(== (var _.2683) _.266)
                      ;(lookupo _.2683 ((closure _.887 ((x))) . _.265) 1))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (app _.2684 _.2685) _.266)
                         ;(eval-expo
                          ;_.2684
                          ;((closure _.887 ((x))) . _.265)
                          ;(closure _.2688 _.2687)))
                        ;(eval-expo
                         ;_.2685
                         ;((closure _.887 ((x))) . _.265)
                         ;_.2686))
                       ;(eval-expo _.2688 (_.2686 . _.2687) 1))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(conj
                          ;(== (cons _.2689 _.2690) _.266)
                          ;(== (_.2691 . _.2692) 1))
                         ;(eval-expo
                          ;_.2689
                          ;((closure _.887 ((x))) . _.265)
                          ;_.2691))
                        ;(eval-expo
                         ;_.2690
                         ;((closure _.887 ((x))) . _.265)
                         ;_.2692))
                       ;(disj
                        ;(conj
                         ;(conj (== (car _.2693) _.266) (== _.2694 1))
                         ;(eval-expo
                          ;_.2693
                          ;((closure _.887 ((x))) . _.265)
                          ;(_.2694 . _.2695)))
                        ;(conj
                         ;(conj (== (cdr _.2696) _.266) (== _.2698 1))
                         ;(eval-expo
                          ;_.2696
                          ;((closure _.887 ((x))) . _.265)
                          ;(_.2697 . _.2698)))))))))))))))
            ;(eval-listo _.233 (_.139 . _.140) _.235))
           ;(conj
            ;(pause
             ;(state ((lambda (list (cdr '(_.275 . 1)) _.1606 . _.1607))))
             ;(disj
              ;(conj (== '_.2522 _.1606) (== _.2522 x))
              ;(disj
               ;(conj (== (list . _.2523) _.1606) (eval-listo _.2523 ((x)) x))
               ;(disj
                ;(conj (== (var _.2524) _.1606) (lookupo _.2524 ((x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.2525 _.2526) _.1606)
                    ;(eval-expo _.2525 ((x)) (closure _.2529 _.2528)))
                   ;(eval-expo _.2526 ((x)) _.2527))
                  ;(eval-expo _.2529 (_.2527 . _.2528) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.2530 _.2531) _.1606)
                     ;(== (_.2532 . _.2533) x))
                    ;(eval-expo _.2530 ((x)) _.2532))
                   ;(eval-expo _.2531 ((x)) _.2533))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.2534) _.1606) (== _.2535 x))
                    ;(eval-expo _.2534 ((x)) (_.2535 . _.2536)))
                   ;(conj
                    ;(conj (== (cdr _.2537) _.1606) (== _.2539 x))
                    ;(eval-expo _.2537 ((x)) (_.2538 . _.2539))))))))))
            ;(eval-listo _.1607 (_.139 . _.140) _.1609)))
          ;(disj
           ;(pause
            ;(state ((lambda (list (car '(1 . _.273)) 'x . _.1496))))
            ;(conj
             ;(conj
              ;(conj (== (_.2471 . _.2472) _.1496) (== (_.2473 . _.2474) ()))
              ;(eval-expo _.2471 ((x)) _.2473))
             ;(eval-listo _.2472 ((x)) _.2474)))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (list (car '(1 . _.273)) _.1495 . _.1496))))
              ;(disj
               ;(conj (== (var _.1628) _.1495) (lookupo _.1628 ((x)) x))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (app _.1629 _.1630) _.1495)
                   ;(eval-expo _.1629 ((x)) (closure _.1633 _.1632)))
                  ;(eval-expo _.1630 ((x)) _.1631))
                 ;(eval-expo _.1633 (_.1631 . _.1632) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (cons _.1634 _.1635) _.1495)
                    ;(== (_.1636 . _.1637) x))
                   ;(eval-expo _.1634 ((x)) _.1636))
                  ;(eval-expo _.1635 ((x)) _.1637))
                 ;(disj
                  ;(conj
                   ;(conj (== (car _.1638) _.1495) (== _.1639 x))
                   ;(eval-expo _.1638 ((x)) (_.1639 . _.1640)))
                  ;(conj
                   ;(conj (== (cdr _.1641) _.1495) (== _.1643 x))
                   ;(eval-expo _.1641 ((x)) (_.1642 . _.1643))))))))
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list (car '(1 . _.273)) (list . _.1627) . _.1496))))
               ;(conj (== () _.1627) (== () x)))
              ;(pause
               ;(state
                ;((lambda (list (car '(1 . _.273)) (list . _.1627) . _.1496))))
               ;(conj
                ;(conj
                 ;(conj (== (_.2981 . _.2982) _.1627) (== (_.2983 . _.2984) x))
                 ;(eval-expo _.2981 ((x)) _.2983))
                ;(eval-listo _.2982 ((x)) _.2984)))))
            ;(eval-listo _.1496 (_.139 . _.140) _.1498))))
         ;(conj
          ;(disj
           ;(pause
            ;(state
             ;((lambda (list
                       ;(app (lambda '1) (lambda _.652))
                       ;_.1449
                       ;.
                       ;_.1450))))
            ;(disj
             ;(conj
              ;(conj
               ;(conj (== (cons _.1550 _.1551) _.1449) (== (_.1552 . _.1553) x))
               ;(eval-expo _.1550 ((x)) _.1552))
              ;(eval-expo _.1551 ((x)) _.1553))
             ;(disj
              ;(conj
               ;(conj (== (car _.1554) _.1449) (== _.1555 x))
               ;(eval-expo _.1554 ((x)) (_.1555 . _.1556)))
              ;(conj
               ;(conj (== (cdr _.1557) _.1449) (== _.1559 x))
               ;(eval-expo _.1557 ((x)) (_.1558 . _.1559))))))
           ;(conj
            ;(conj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;(app (lambda '1) (lambda _.652))
                          ;(app _.1545 _.1546)
                          ;.
                          ;_.1450))))
               ;(conj
                ;(== (lambda _.2924) _.1545)
                ;(== (closure _.2924 ((x))) (closure _.1549 _.1548))))
              ;(pause
               ;(state
                ;((lambda (list
                          ;(app (lambda '1) (lambda _.652))
                          ;(app _.1545 _.1546)
                          ;.
                          ;_.1450))))
               ;(disj
                ;(conj (== '_.2925 _.1545) (== _.2925 (closure _.1549 _.1548)))
                ;(disj
                 ;(conj
                  ;(== (list . _.2926) _.1545)
                  ;(eval-listo _.2926 ((x)) (closure _.1549 _.1548)))
                 ;(disj
                  ;(conj
                   ;(== (var _.2927) _.1545)
                   ;(lookupo _.2927 ((x)) (closure _.1549 _.1548)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.2928 _.2929) _.1545)
                      ;(eval-expo _.2928 ((x)) (closure _.2932 _.2931)))
                     ;(eval-expo _.2929 ((x)) _.2930))
                    ;(eval-expo
                     ;_.2932
                     ;(_.2930 . _.2931)
                     ;(closure _.1549 _.1548)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.2933 _.2934) _.1545)
                       ;(== (_.2935 . _.2936) (closure _.1549 _.1548)))
                      ;(eval-expo _.2933 ((x)) _.2935))
                     ;(eval-expo _.2934 ((x)) _.2936))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.2937) _.1545)
                       ;(== _.2938 (closure _.1549 _.1548)))
                      ;(eval-expo _.2937 ((x)) (_.2938 . _.2939)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.2940) _.1545)
                       ;(== _.2942 (closure _.1549 _.1548)))
                      ;(eval-expo _.2940 ((x)) (_.2941 . _.2942)))))))))))
             ;(eval-expo _.1546 (_.139 . _.140) _.1547))
            ;(eval-expo _.1549 (_.1547 . _.1548) _.1451)))
          ;(eval-listo _.1450 (_.139 . _.140) _.1452)))
        ;(conj
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app '(closure _.391 _.390) (lambda _.1347))
                          ;.
                          ;_.335))))
               ;(conj
                ;(== (lambda _.2372) _.391)
                ;(== (closure _.2372 ((closure _.1347 ((x))) . _.390)) x)))
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app '(closure _.391 _.390) (lambda _.1347))
                          ;.
                          ;_.335))))
               ;(disj
                ;(conj (== '_.2373 _.391) (== _.2373 x))
                ;(disj
                 ;(conj
                  ;(== (list . _.2374) _.391)
                  ;(eval-listo _.2374 ((closure _.1347 ((x))) . _.390) x))
                 ;(disj
                  ;(conj
                   ;(== (var _.2375) _.391)
                   ;(lookupo _.2375 ((closure _.1347 ((x))) . _.390) x))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.2376 _.2377) _.391)
                      ;(eval-expo
                       ;_.2376
                       ;((closure _.1347 ((x))) . _.390)
                       ;(closure _.2380 _.2379)))
                     ;(eval-expo
                      ;_.2377
                      ;((closure _.1347 ((x))) . _.390)
                      ;_.2378))
                    ;(eval-expo _.2380 (_.2378 . _.2379) x))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.2381 _.2382) _.391)
                       ;(== (_.2383 . _.2384) x))
                      ;(eval-expo
                       ;_.2381
                       ;((closure _.1347 ((x))) . _.390)
                       ;_.2383))
                     ;(eval-expo
                      ;_.2382
                      ;((closure _.1347 ((x))) . _.390)
                      ;_.2384))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.2385) _.391) (== _.2386 x))
                      ;(eval-expo
                       ;_.2385
                       ;((closure _.1347 ((x))) . _.390)
                       ;(_.2386 . _.2387)))
                     ;(conj
                      ;(conj (== (cdr _.2388) _.391) (== _.2390 x))
                      ;(eval-expo
                       ;_.2388
                       ;((closure _.1347 ((x))) . _.390)
                       ;(_.2389 . _.2390)))))))))))
             ;(conj
              ;(disj
               ;(disj
                ;(conj
                 ;(disj
                  ;(pause
                   ;(state ((lambda (list '1 (app _.387 _.388) . _.335))))
                   ;(conj
                    ;(== (list . _.806) _.387)
                    ;(eval-listo _.806 ((x)) (closure _.391 _.390))))
                  ;(pause
                   ;(state ((lambda (list '1 (app _.387 _.388) . _.335))))
                   ;(disj
                    ;(conj
                     ;(== (var _.807) _.387)
                     ;(lookupo _.807 ((x)) (closure _.391 _.390)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.808 _.809) _.387)
                        ;(eval-expo _.808 ((x)) (closure _.812 _.811)))
                       ;(eval-expo _.809 ((x)) _.810))
                      ;(eval-expo _.812 (_.810 . _.811) (closure _.391 _.390)))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.813 _.814) _.387)
                         ;(== (_.815 . _.816) (closure _.391 _.390)))
                        ;(eval-expo _.813 ((x)) _.815))
                       ;(eval-expo _.814 ((x)) _.816))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(== (car _.817) _.387)
                         ;(== _.818 (closure _.391 _.390)))
                        ;(eval-expo _.817 ((x)) (_.818 . _.819)))
                       ;(conj
                        ;(conj
                         ;(== (cdr _.820) _.387)
                         ;(== _.822 (closure _.391 _.390)))
                        ;(eval-expo _.820 ((x)) (_.821 . _.822)))))))))
                 ;(eval-expo _.388 (_.139 . _.140) _.389))
                ;(pause
                 ;(state
                  ;((lambda (list
                            ;'1
                            ;(app '(closure _.391 _.390) _.388)
                            ;.
                            ;_.335))))
                 ;(disj
                  ;(conj (== '_.1348 _.388) (== _.1348 _.389))
                  ;(disj
                   ;(conj
                    ;(== (list . _.1349) _.388)
                    ;(eval-listo _.1349 ((x)) _.389))
                   ;(disj
                    ;(conj (== (var _.1350) _.388) (lookupo _.1350 ((x)) _.389))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.1351 _.1352) _.388)
                        ;(eval-expo _.1351 ((x)) (closure _.1355 _.1354)))
                       ;(eval-expo _.1352 ((x)) _.1353))
                      ;(eval-expo _.1355 (_.1353 . _.1354) _.389))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.1356 _.1357) _.388)
                         ;(== (_.1358 . _.1359) _.389))
                        ;(eval-expo _.1356 ((x)) _.1358))
                       ;(eval-expo _.1357 ((x)) _.1359))
                      ;(disj
                       ;(conj
                        ;(conj (== (car _.1360) _.388) (== _.1361 _.389))
                        ;(eval-expo _.1360 ((x)) (_.1361 . _.1362)))
                       ;(conj
                        ;(conj (== (cdr _.1363) _.388) (== _.1365 _.389))
                        ;(eval-expo _.1363 ((x)) (_.1364 . _.1365)))))))))))
               ;(disj
                ;(pause
                 ;(state
                  ;((lambda (list '1 (app (lambda _.391) _.388) . _.335))))
                 ;(conj
                  ;(== (list . _.969) _.388)
                  ;(eval-listo _.969 ((x)) _.389)))
                ;(pause
                 ;(state
                  ;((lambda (list '1 (app (lambda _.391) _.388) . _.335))))
                 ;(disj
                  ;(conj (== (var _.970) _.388) (lookupo _.970 ((x)) _.389))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.971 _.972) _.388)
                      ;(eval-expo _.971 ((x)) (closure _.975 _.974)))
                     ;(eval-expo _.972 ((x)) _.973))
                    ;(eval-expo _.975 (_.973 . _.974) _.389))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.976 _.977) _.388)
                       ;(== (_.978 . _.979) _.389))
                      ;(eval-expo _.976 ((x)) _.978))
                     ;(eval-expo _.977 ((x)) _.979))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.980) _.388) (== _.981 _.389))
                      ;(eval-expo _.980 ((x)) (_.981 . _.982)))
                     ;(conj
                      ;(conj (== (cdr _.983) _.388) (== _.985 _.389))
                      ;(eval-expo _.983 ((x)) (_.984 . _.985))))))))))
              ;(eval-expo _.391 (_.389 . _.390) _.336)))
            ;(disj
             ;(pause
              ;(state ((lambda (list '1 (app (lambda _.391) '_.389) . _.335))))
              ;(conj (== '_.1713 _.391) (== _.1713 x)))
             ;(pause
              ;(state ((lambda (list '1 (app (lambda _.391) '_.389) . _.335))))
              ;(disj
               ;(conj
                ;(== (list . _.1714) _.391)
                ;(eval-listo _.1714 (_.389 (x)) x))
               ;(disj
                ;(conj (== (var _.1715) _.391) (lookupo _.1715 (_.389 (x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.1716 _.1717) _.391)
                    ;(eval-expo _.1716 (_.389 (x)) (closure _.1720 _.1719)))
                   ;(eval-expo _.1717 (_.389 (x)) _.1718))
                  ;(eval-expo _.1720 (_.1718 . _.1719) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.1721 _.1722) _.391)
                     ;(== (_.1723 . _.1724) x))
                    ;(eval-expo _.1721 (_.389 (x)) _.1723))
                   ;(eval-expo _.1722 (_.389 (x)) _.1724))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.1725) _.391) (== _.1726 x))
                    ;(eval-expo _.1725 (_.389 (x)) (_.1726 . _.1727)))
                   ;(conj
                    ;(conj (== (cdr _.1728) _.391) (== _.1730 x))
                    ;(eval-expo _.1728 (_.389 (x)) (_.1729 . _.1730)))))))))))
           ;(disj
            ;(pause
             ;(state
              ;((lambda (list
                        ;'1
                        ;(app (lambda (list . _.1265)) (lambda _.967))
                        ;.
                        ;_.335))))
             ;(conj
              ;(conj
               ;(conj (== (_.2475 . _.2476) _.1265) (== (_.2477 . _.2478) x))
               ;(eval-expo _.2475 ((closure _.967 ((x))) (x)) _.2477))
              ;(eval-listo _.2476 ((closure _.967 ((x))) (x)) _.2478)))
            ;(disj
             ;(pause
              ;(state
               ;((lambda (list
                         ;'1
                         ;(app (lambda _.391) (lambda _.967))
                         ;.
                         ;_.335))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.1267 _.1268) _.391)
                  ;(eval-expo
                   ;_.1267
                   ;((closure _.967 ((x))) (x))
                   ;(closure _.1271 _.1270)))
                 ;(eval-expo _.1268 ((closure _.967 ((x))) (x)) _.1269))
                ;(eval-expo _.1271 (_.1269 . _.1270) x))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.1272 _.1273) _.391)
                   ;(== (_.1274 . _.1275) x))
                  ;(eval-expo _.1272 ((closure _.967 ((x))) (x)) _.1274))
                 ;(eval-expo _.1273 ((closure _.967 ((x))) (x)) _.1275))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.1276) _.391) (== _.1277 x))
                  ;(eval-expo
                   ;_.1276
                   ;((closure _.967 ((x))) (x))
                   ;(_.1277 . _.1278)))
                 ;(conj
                  ;(conj (== (cdr _.1279) _.391) (== _.1281 x))
                  ;(eval-expo
                   ;_.1279
                   ;((closure _.967 ((x))) (x))
                   ;(_.1280 . _.1281)))))))
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app (lambda (var _.1266)) (lambda _.967))
                          ;.
                          ;_.335))))
               ;(conj (== () _.1266) (== (closure _.967 ((x))) x)))
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app (lambda (var _.1266)) (lambda _.967))
                          ;.
                          ;_.335))))
               ;(conj (== (s . _.3025) _.1266) (lookupo _.3025 ((x)) x)))))))
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (list '1 (cdr _.399) . _.335))))
              ;(conj
               ;(== (var _.1410) _.399)
               ;(lookupo _.1410 ((x)) (_.400 . x))))
             ;(pause
              ;(state ((lambda (list '1 (cdr _.399) . _.335))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.1411 _.1412) _.399)
                  ;(eval-expo _.1411 ((x)) (closure _.1415 _.1414)))
                 ;(eval-expo _.1412 ((x)) _.1413))
                ;(eval-expo _.1415 (_.1413 . _.1414) (_.400 . x)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.1416 _.1417) _.399)
                   ;(== (_.1418 . _.1419) (_.400 . x)))
                  ;(eval-expo _.1416 ((x)) _.1418))
                 ;(eval-expo _.1417 ((x)) _.1419))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.1420) _.399) (== _.1421 (_.400 . x)))
                  ;(eval-expo _.1420 ((x)) (_.1421 . _.1422)))
                 ;(conj
                  ;(conj (== (cdr _.1423) _.399) (== _.1425 (_.400 . x)))
                  ;(eval-expo _.1423 ((x)) (_.1424 . _.1425))))))))
            ;(pause
             ;(state ((lambda (list '1 (cdr (list . _.1409)) . _.335))))
             ;(conj
              ;(conj
               ;(conj
                ;(== (_.2588 . _.2589) _.1409)
                ;(== (_.2590 . _.2591) (_.400 . x)))
               ;(eval-expo _.2588 ((x)) _.2590))
              ;(eval-listo _.2589 ((x)) _.2591))))
           ;(disj
            ;(pause
             ;(state ((lambda (list '1 (car (list . _.1284)) . _.335))))
             ;(conj
              ;(conj
               ;(conj
                ;(== (_.2517 . _.2518) _.1284)
                ;(== (_.2519 . _.2520) (x . _.398)))
               ;(eval-expo _.2517 ((x)) _.2519))
              ;(eval-listo _.2518 ((x)) _.2520)))
            ;(disj
             ;(pause
              ;(state ((lambda (list '1 (car _.396) . _.335))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.1286 _.1287) _.396)
                  ;(eval-expo _.1286 ((x)) (closure _.1290 _.1289)))
                 ;(eval-expo _.1287 ((x)) _.1288))
                ;(eval-expo _.1290 (_.1288 . _.1289) (x . _.398)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.1291 _.1292) _.396)
                   ;(== (_.1293 . _.1294) (x . _.398)))
                  ;(eval-expo _.1291 ((x)) _.1293))
                 ;(eval-expo _.1292 ((x)) _.1294))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.1295) _.396) (== _.1296 (x . _.398)))
                  ;(eval-expo _.1295 ((x)) (_.1296 . _.1297)))
                 ;(conj
                  ;(conj (== (cdr _.1298) _.396) (== _.1300 (x . _.398)))
                  ;(eval-expo _.1298 ((x)) (_.1299 . _.1300)))))))
             ;(disj
              ;(pause
               ;(state ((lambda (list '1 (car (var _.1285)) . _.335))))
               ;(conj (== () _.1285) (== (x) (x . _.398))))
              ;(pause
               ;(state ((lambda (list '1 (car (var _.1285)) . _.335))))
               ;(conj
                ;(== (s . _.3073) _.1285)
                ;(lookupo _.3073 () (x . _.398)))))))))
         ;(eval-listo _.335 (_.139 . _.140) _.337)))
       ;(disj
        ;(disj
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cdr _.206))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.470 _.471) _.206)
                 ;(eval-expo _.470 ((x)) (closure _.474 _.473)))
                ;(eval-expo _.471 ((x)) _.472))
               ;(eval-expo _.474 (_.472 . _.473) (_.207 1 x))))
             ;(pause
              ;(state ((lambda (cdr _.206))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.475 _.476) _.206)
                  ;(== (_.477 . _.478) (_.207 1 x)))
                 ;(eval-expo _.475 ((x)) _.477))
                ;(eval-expo _.476 ((x)) _.478))
               ;(disj
                ;(conj
                 ;(conj (== (car _.479) _.206) (== _.480 (_.207 1 x)))
                 ;(eval-expo _.479 ((x)) (_.480 . _.481)))
                ;(conj
                 ;(conj (== (cdr _.482) _.206) (== _.484 (_.207 1 x)))
                 ;(eval-expo _.482 ((x)) (_.483 . _.484)))))))
            ;(pause
             ;(state ((lambda (cdr (var _.469)))))
             ;(conj (== (s . _.1771) _.469) (lookupo _.1771 () (_.207 1 x)))))
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cdr (list (lambda _.2041) . _.1427)))))
              ;(conj (== () _.1427) (== () (1 x))))
             ;(pause
              ;(state ((lambda (cdr (list (lambda _.2041) . _.1427)))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (_.2540 . _.2541) _.1427)
                 ;(== (_.2542 . _.2543) (1 x)))
                ;(eval-expo _.2540 ((x)) _.2542))
               ;(eval-listo _.2541 ((x)) _.2543))))
            ;(conj
             ;(disj
              ;(pause
               ;(state ((lambda (cdr (list _.1426 . _.1427)))))
               ;(conj (== '_.2042 _.1426) (== _.2042 _.207)))
              ;(pause
               ;(state ((lambda (cdr (list _.1426 . _.1427)))))
               ;(disj
                ;(conj
                 ;(== (list . _.2043) _.1426)
                 ;(eval-listo _.2043 ((x)) _.207))
                ;(disj
                 ;(conj (== (var _.2044) _.1426) (lookupo _.2044 ((x)) _.207))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.2045 _.2046) _.1426)
                     ;(eval-expo _.2045 ((x)) (closure _.2049 _.2048)))
                    ;(eval-expo _.2046 ((x)) _.2047))
                   ;(eval-expo _.2049 (_.2047 . _.2048) _.207))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.2050 _.2051) _.1426)
                      ;(== (_.2052 . _.2053) _.207))
                     ;(eval-expo _.2050 ((x)) _.2052))
                    ;(eval-expo _.2051 ((x)) _.2053))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.2054) _.1426) (== _.2055 _.207))
                     ;(eval-expo _.2054 ((x)) (_.2055 . _.2056)))
                    ;(conj
                     ;(conj (== (cdr _.2057) _.1426) (== _.2059 _.207))
                     ;(eval-expo _.2057 ((x)) (_.2058 . _.2059))))))))))
             ;(eval-listo _.1427 (_.139 . _.140) _.1429))))
          ;(disj
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (car (list _.1343 . _.1344)))))
              ;(conj (== '_.1856 _.1343) (== _.1856 (1 x))))
             ;(pause
              ;(state ((lambda (car (list _.1343 . _.1344)))))
              ;(disj
               ;(conj
                ;(== (list . _.1857) _.1343)
                ;(eval-listo _.1857 ((x)) (1 x)))
               ;(disj
                ;(conj (== (var _.1858) _.1343) (lookupo _.1858 ((x)) (1 x)))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.1859 _.1860) _.1343)
                    ;(eval-expo _.1859 ((x)) (closure _.1863 _.1862)))
                   ;(eval-expo _.1860 ((x)) _.1861))
                  ;(eval-expo _.1863 (_.1861 . _.1862) (1 x)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.1864 _.1865) _.1343)
                     ;(== (_.1866 . _.1867) (1 x)))
                    ;(eval-expo _.1864 ((x)) _.1866))
                   ;(eval-expo _.1865 ((x)) _.1867))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.1868) _.1343) (== _.1869 (1 x)))
                    ;(eval-expo _.1868 ((x)) (_.1869 . _.1870)))
                   ;(conj
                    ;(conj (== (cdr _.1871) _.1343) (== _.1873 (1 x)))
                    ;(eval-expo _.1871 ((x)) (_.1872 . _.1873))))))))))
            ;(eval-listo _.1344 (_.139 . _.140) _.1346))
           ;(disj
            ;(pause
             ;(state ((lambda (car (var _.446)))))
             ;(conj
              ;(== (s . _.1646) _.446)
              ;(lookupo _.1646 () ((1 x) . _.205))))
            ;(disj
             ;(pause
              ;(state ((lambda (car _.203))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.452 _.453) _.203)
                  ;(== (_.454 . _.455) ((1 x) . _.205)))
                 ;(eval-expo _.452 ((x)) _.454))
                ;(eval-expo _.453 ((x)) _.455))
               ;(disj
                ;(conj
                 ;(conj (== (car _.456) _.203) (== _.457 ((1 x) . _.205)))
                 ;(eval-expo _.456 ((x)) (_.457 . _.458)))
                ;(conj
                 ;(conj (== (cdr _.459) _.203) (== _.461 ((1 x) . _.205)))
                 ;(eval-expo _.459 ((x)) (_.460 . _.461))))))
             ;(conj
              ;(conj
               ;(disj
                ;(pause
                 ;(state ((lambda (car (app _.447 _.448)))))
                 ;(conj
                  ;(== (lambda _.2985) _.447)
                  ;(== (closure _.2985 ((x))) (closure _.451 _.450))))
                ;(pause
                 ;(state ((lambda (car (app _.447 _.448)))))
                 ;(disj
                  ;(conj (== '_.2986 _.447) (== _.2986 (closure _.451 _.450)))
                  ;(disj
                   ;(conj
                    ;(== (list . _.2987) _.447)
                    ;(eval-listo _.2987 ((x)) (closure _.451 _.450)))
                   ;(disj
                    ;(conj
                     ;(== (var _.2988) _.447)
                     ;(lookupo _.2988 ((x)) (closure _.451 _.450)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.2989 _.2990) _.447)
                        ;(eval-expo _.2989 ((x)) (closure _.2993 _.2992)))
                       ;(eval-expo _.2990 ((x)) _.2991))
                      ;(eval-expo
                       ;_.2993
                       ;(_.2991 . _.2992)
                       ;(closure _.451 _.450)))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.2994 _.2995) _.447)
                         ;(== (_.2996 . _.2997) (closure _.451 _.450)))
                        ;(eval-expo _.2994 ((x)) _.2996))
                       ;(eval-expo _.2995 ((x)) _.2997))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(== (car _.2998) _.447)
                         ;(== _.2999 (closure _.451 _.450)))
                        ;(eval-expo _.2998 ((x)) (_.2999 . _.3000)))
                       ;(conj
                        ;(conj
                         ;(== (cdr _.3001) _.447)
                         ;(== _.3003 (closure _.451 _.450)))
                        ;(eval-expo _.3001 ((x)) (_.3002 . _.3003)))))))))))
               ;(eval-expo _.448 (_.139 . _.140) _.449))
              ;(eval-expo _.451 (_.449 . _.450) (_.204 . _.205)))))))
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cons '1 _.200))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.512 _.513) _.200)
                 ;(eval-expo _.512 ((x)) (closure _.516 _.515)))
                ;(eval-expo _.513 ((x)) _.514))
               ;(eval-expo _.516 (_.514 . _.515) (x))))
             ;(pause
              ;(state ((lambda (cons '1 _.200))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj (== (cons _.517 _.518) _.200) (== (_.519 . _.520) (x)))
                 ;(eval-expo _.517 ((x)) _.519))
                ;(eval-expo _.518 ((x)) _.520))
               ;(disj
                ;(conj
                 ;(conj (== (car _.521) _.200) (== _.522 (x)))
                 ;(eval-expo _.521 ((x)) (_.522 . _.523)))
                ;(conj
                 ;(conj (== (cdr _.524) _.200) (== _.526 (x)))
                 ;(eval-expo _.524 ((x)) (_.525 . _.526)))))))
            ;(pause
             ;(state ((lambda (cons '1 (var _.511)))))
             ;(conj (== (s . _.1793) _.511) (lookupo _.1793 () (x)))))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (cons '1 (list _.1453 . _.1454)))))
              ;(conj (== '_.2088 _.1453) (== _.2088 x)))
             ;(pause
              ;(state ((lambda (cons '1 (list _.1453 . _.1454)))))
              ;(disj
               ;(conj (== (list . _.2089) _.1453) (eval-listo _.2089 ((x)) x))
               ;(disj
                ;(conj (== (var _.2090) _.1453) (lookupo _.2090 ((x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.2091 _.2092) _.1453)
                    ;(eval-expo _.2091 ((x)) (closure _.2095 _.2094)))
                   ;(eval-expo _.2092 ((x)) _.2093))
                  ;(eval-expo _.2095 (_.2093 . _.2094) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.2096 _.2097) _.1453)
                     ;(== (_.2098 . _.2099) x))
                    ;(eval-expo _.2096 ((x)) _.2098))
                   ;(eval-expo _.2097 ((x)) _.2099))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.2100) _.1453) (== _.2101 x))
                    ;(eval-expo _.2100 ((x)) (_.2101 . _.2102)))
                   ;(conj
                    ;(conj (== (cdr _.2103) _.1453) (== _.2105 x))
                    ;(eval-expo _.2103 ((x)) (_.2104 . _.2105))))))))))
            ;(eval-listo _.1454 (_.139 . _.140) _.1456)))
          ;(conj
           ;(disj
            ;(pause
             ;(state ((lambda (cons _.199 _.200))))
             ;(disj
              ;(conj
               ;(conj (== (car _.377) _.199) (== _.378 1))
               ;(eval-expo _.377 ((x)) (_.378 . _.379)))
              ;(conj
               ;(conj (== (cdr _.380) _.199) (== _.382 1))
               ;(eval-expo _.380 ((x)) (_.381 . _.382)))))
            ;(conj
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (cons (app (lambda _.372) _.369) _.200))))
                ;(conj
                 ;(== (lambda _.2623) _.369)
                 ;(== (closure _.2623 ((x))) _.370)))
               ;(pause
                ;(state ((lambda (cons (app (lambda _.372) _.369) _.200))))
                ;(disj
                 ;(conj (== '_.2624 _.369) (== _.2624 _.370))
                 ;(disj
                  ;(conj
                   ;(== (list . _.2625) _.369)
                   ;(eval-listo _.2625 ((x)) _.370))
                  ;(disj
                   ;(conj (== (var _.2626) _.369) (lookupo _.2626 ((x)) _.370))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (app _.2627 _.2628) _.369)
                       ;(eval-expo _.2627 ((x)) (closure _.2631 _.2630)))
                      ;(eval-expo _.2628 ((x)) _.2629))
                     ;(eval-expo _.2631 (_.2629 . _.2630) _.370))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (cons _.2632 _.2633) _.369)
                        ;(== (_.2634 . _.2635) _.370))
                       ;(eval-expo _.2632 ((x)) _.2634))
                      ;(eval-expo _.2633 ((x)) _.2635))
                     ;(disj
                      ;(conj
                       ;(conj (== (car _.2636) _.369) (== _.2637 _.370))
                       ;(eval-expo _.2636 ((x)) (_.2637 . _.2638)))
                      ;(conj
                       ;(conj (== (cdr _.2639) _.369) (== _.2641 _.370))
                       ;(eval-expo _.2639 ((x)) (_.2640 . _.2641)))))))))))
              ;(conj
               ;(disj
                ;(pause
                 ;(state ((lambda (cons (app _.368 _.369) _.200))))
                 ;(conj (== '_.1921 _.368) (== _.1921 (closure _.372 _.371))))
                ;(pause
                 ;(state ((lambda (cons (app _.368 _.369) _.200))))
                 ;(disj
                  ;(conj
                   ;(== (list . _.1922) _.368)
                   ;(eval-listo _.1922 ((x)) (closure _.372 _.371)))
                  ;(disj
                   ;(conj
                    ;(== (var _.1923) _.368)
                    ;(lookupo _.1923 ((x)) (closure _.372 _.371)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (app _.1924 _.1925) _.368)
                       ;(eval-expo _.1924 ((x)) (closure _.1928 _.1927)))
                      ;(eval-expo _.1925 ((x)) _.1926))
                     ;(eval-expo
                      ;_.1928
                      ;(_.1926 . _.1927)
                      ;(closure _.372 _.371)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (cons _.1929 _.1930) _.368)
                        ;(== (_.1931 . _.1932) (closure _.372 _.371)))
                       ;(eval-expo _.1929 ((x)) _.1931))
                      ;(eval-expo _.1930 ((x)) _.1932))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(== (car _.1933) _.368)
                        ;(== _.1934 (closure _.372 _.371)))
                       ;(eval-expo _.1933 ((x)) (_.1934 . _.1935)))
                      ;(conj
                       ;(conj
                        ;(== (cdr _.1936) _.368)
                        ;(== _.1938 (closure _.372 _.371)))
                       ;(eval-expo _.1936 ((x)) (_.1937 . _.1938))))))))))
               ;(eval-expo _.369 (_.139 . _.140) _.370)))
             ;(eval-expo _.372 (_.370 . _.371) _.201)))
           ;(eval-expo _.200 (_.139 . _.140) _.202))))
        ;(disj
         ;(disj
          ;(disj
           ;(disj
            ;(pause
             ;(state ((lambda (app (lambda _.198) (lambda _.341)))))
             ;(disj
              ;(conj
               ;(conj (== (car _.415) _.198) (== _.416 (1 x)))
               ;(eval-expo _.415 ((closure _.341 ((x))) (x)) (_.416 . _.417)))
              ;(conj
               ;(conj (== (cdr _.418) _.198) (== _.420 (1 x)))
               ;(eval-expo _.418 ((closure _.341 ((x))) (x)) (_.419 . _.420)))))
            ;(conj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app (lambda (cons _.411 _.412)) (lambda _.341)))))
               ;(conj
                ;(== (lambda _.2699) _.411)
                ;(== (closure _.2699 ((closure _.341 ((x))) (x))) 1)))
              ;(pause
               ;(state
                ;((lambda (app (lambda (cons _.411 _.412)) (lambda _.341)))))
               ;(disj
                ;(conj (== '_.2700 _.411) (== _.2700 1))
                ;(disj
                 ;(conj
                  ;(== (list . _.2701) _.411)
                  ;(eval-listo _.2701 ((closure _.341 ((x))) (x)) 1))
                 ;(disj
                  ;(conj
                   ;(== (var _.2702) _.411)
                   ;(lookupo _.2702 ((closure _.341 ((x))) (x)) 1))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.2703 _.2704) _.411)
                      ;(eval-expo
                       ;_.2703
                       ;((closure _.341 ((x))) (x))
                       ;(closure _.2707 _.2706)))
                     ;(eval-expo _.2704 ((closure _.341 ((x))) (x)) _.2705))
                    ;(eval-expo _.2707 (_.2705 . _.2706) 1))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.2708 _.2709) _.411)
                       ;(== (_.2710 . _.2711) 1))
                      ;(eval-expo _.2708 ((closure _.341 ((x))) (x)) _.2710))
                     ;(eval-expo _.2709 ((closure _.341 ((x))) (x)) _.2711))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.2712) _.411) (== _.2713 1))
                      ;(eval-expo
                       ;_.2712
                       ;((closure _.341 ((x))) (x))
                       ;(_.2713 . _.2714)))
                     ;(conj
                      ;(conj (== (cdr _.2715) _.411) (== _.2717 1))
                      ;(eval-expo
                       ;_.2715
                       ;((closure _.341 ((x))) (x))
                       ;(_.2716 . _.2717)))))))))))
             ;(eval-expo _.412 (_.196 . _.197) _.414)))
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (app (lambda _.410) _.407))
                          ;(lambda _.341)))))
               ;(conj
                ;(== (lambda _.2395) _.407)
                ;(== (closure _.2395 ((closure _.341 ((x))) (x))) _.408)))
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (app (lambda _.410) _.407))
                          ;(lambda _.341)))))
               ;(disj
                ;(conj (== '_.2396 _.407) (== _.2396 _.408))
                ;(disj
                 ;(conj
                  ;(== (list . _.2397) _.407)
                  ;(eval-listo _.2397 ((closure _.341 ((x))) (x)) _.408))
                 ;(disj
                  ;(conj
                   ;(== (var _.2398) _.407)
                   ;(lookupo _.2398 ((closure _.341 ((x))) (x)) _.408))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.2399 _.2400) _.407)
                      ;(eval-expo
                       ;_.2399
                       ;((closure _.341 ((x))) (x))
                       ;(closure _.2403 _.2402)))
                     ;(eval-expo _.2400 ((closure _.341 ((x))) (x)) _.2401))
                    ;(eval-expo _.2403 (_.2401 . _.2402) _.408))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.2404 _.2405) _.407)
                       ;(== (_.2406 . _.2407) _.408))
                      ;(eval-expo _.2404 ((closure _.341 ((x))) (x)) _.2406))
                     ;(eval-expo _.2405 ((closure _.341 ((x))) (x)) _.2407))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.2408) _.407) (== _.2409 _.408))
                      ;(eval-expo
                       ;_.2408
                       ;((closure _.341 ((x))) (x))
                       ;(_.2409 . _.2410)))
                     ;(conj
                      ;(conj (== (cdr _.2411) _.407) (== _.2413 _.408))
                      ;(eval-expo
                       ;_.2411
                       ;((closure _.341 ((x))) (x))
                       ;(_.2412 . _.2413)))))))))))
             ;(conj
              ;(disj
               ;(pause
                ;(state
                 ;((lambda (app (lambda (app _.406 _.407)) (lambda _.341)))))
                ;(conj (== '_.1523 _.406) (== _.1523 (closure _.410 _.409))))
               ;(pause
                ;(state
                 ;((lambda (app (lambda (app _.406 _.407)) (lambda _.341)))))
                ;(disj
                 ;(conj
                  ;(== (list . _.1524) _.406)
                  ;(eval-listo
                   ;_.1524
                   ;((closure _.341 ((x))) (x))
                   ;(closure _.410 _.409)))
                 ;(disj
                  ;(conj
                   ;(== (var _.1525) _.406)
                   ;(lookupo
                    ;_.1525
                    ;((closure _.341 ((x))) (x))
                    ;(closure _.410 _.409)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.1526 _.1527) _.406)
                      ;(eval-expo
                       ;_.1526
                       ;((closure _.341 ((x))) (x))
                       ;(closure _.1530 _.1529)))
                     ;(eval-expo _.1527 ((closure _.341 ((x))) (x)) _.1528))
                    ;(eval-expo _.1530 (_.1528 . _.1529) (closure _.410 _.409)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.1531 _.1532) _.406)
                       ;(== (_.1533 . _.1534) (closure _.410 _.409)))
                      ;(eval-expo _.1531 ((closure _.341 ((x))) (x)) _.1533))
                     ;(eval-expo _.1532 ((closure _.341 ((x))) (x)) _.1534))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.1535) _.406)
                       ;(== _.1536 (closure _.410 _.409)))
                      ;(eval-expo
                       ;_.1535
                       ;((closure _.341 ((x))) (x))
                       ;(_.1536 . _.1537)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.1538) _.406)
                       ;(== _.1540 (closure _.410 _.409)))
                      ;(eval-expo
                       ;_.1538
                       ;((closure _.341 ((x))) (x))
                       ;(_.1539 . _.1540))))))))))
              ;(eval-expo _.407 (_.196 . _.197) _.408)))
            ;(eval-expo _.410 (_.408 . _.409) _.131)))
          ;(disj
           ;(conj
            ;(pause
             ;(state
              ;((lambda (app
                        ;(lambda (list '1 _.1583 . _.1584))
                        ;(lambda _.341)))))
             ;(disj
              ;(conj (== '_.2480 _.1583) (== _.2480 x))
              ;(disj
               ;(conj
                ;(== (list . _.2481) _.1583)
                ;(eval-listo _.2481 ((closure _.341 ((x))) (x)) x))
               ;(disj
                ;(conj
                 ;(== (var _.2482) _.1583)
                 ;(lookupo _.2482 ((closure _.341 ((x))) (x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.2483 _.2484) _.1583)
                    ;(eval-expo
                     ;_.2483
                     ;((closure _.341 ((x))) (x))
                     ;(closure _.2487 _.2486)))
                   ;(eval-expo _.2484 ((closure _.341 ((x))) (x)) _.2485))
                  ;(eval-expo _.2487 (_.2485 . _.2486) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.2488 _.2489) _.1583)
                     ;(== (_.2490 . _.2491) x))
                    ;(eval-expo _.2488 ((closure _.341 ((x))) (x)) _.2490))
                   ;(eval-expo _.2489 ((closure _.341 ((x))) (x)) _.2491))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.2492) _.1583) (== _.2493 x))
                    ;(eval-expo
                     ;_.2492
                     ;((closure _.341 ((x))) (x))
                     ;(_.2493 . _.2494)))
                   ;(conj
                    ;(conj (== (cdr _.2495) _.1583) (== _.2497 x))
                    ;(eval-expo
                     ;_.2495
                     ;((closure _.341 ((x))) (x))
                     ;(_.2496 . _.2497))))))))))
            ;(eval-listo _.1584 (_.196 . _.197) _.1586))
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app (lambda (list _.735 . _.736)) (lambda _.341)))))
               ;(conj
                ;(== (var _.1148) _.735)
                ;(lookupo _.1148 ((closure _.341 ((x))) (x)) 1)))
              ;(pause
               ;(state
                ;((lambda (app (lambda (list _.735 . _.736)) (lambda _.341)))))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (app _.1149 _.1150) _.735)
                   ;(eval-expo
                    ;_.1149
                    ;((closure _.341 ((x))) (x))
                    ;(closure _.1153 _.1152)))
                  ;(eval-expo _.1150 ((closure _.341 ((x))) (x)) _.1151))
                 ;(eval-expo _.1153 (_.1151 . _.1152) 1))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (cons _.1154 _.1155) _.735)
                    ;(== (_.1156 . _.1157) 1))
                   ;(eval-expo _.1154 ((closure _.341 ((x))) (x)) _.1156))
                  ;(eval-expo _.1155 ((closure _.341 ((x))) (x)) _.1157))
                 ;(disj
                  ;(conj
                   ;(conj (== (car _.1158) _.735) (== _.1159 1))
                   ;(eval-expo
                    ;_.1158
                    ;((closure _.341 ((x))) (x))
                    ;(_.1159 . _.1160)))
                  ;(conj
                   ;(conj (== (cdr _.1161) _.735) (== _.1163 1))
                   ;(eval-expo
                    ;_.1161
                    ;((closure _.341 ((x))) (x))
                    ;(_.1162 . _.1163))))))))
             ;(pause
              ;(state
               ;((lambda (app
                         ;(lambda (list (list . _.1147) . _.736))
                         ;(lambda _.341)))))
              ;(conj
               ;(conj
                ;(conj (== (_.2285 . _.2286) _.1147) (== (_.2287 . _.2288) 1))
                ;(eval-expo _.2285 ((closure _.341 ((x))) (x)) _.2287))
               ;(eval-listo _.2286 ((closure _.341 ((x))) (x)) _.2288))))
            ;(eval-listo _.736 (_.196 . _.197) _.738))))
         ;(disj
          ;(disj
           ;(conj
            ;(disj
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (app (lambda _.198) (list . _.343)))))
                ;(conj (== () _.343) (== () _.196)))
               ;(pause
                ;(state ((lambda (app (lambda _.198) (list . _.343)))))
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (_.1560 . _.1561) _.343)
                   ;(== (_.1562 . _.1563) _.196))
                  ;(eval-expo _.1560 ((x)) _.1562))
                 ;(eval-listo _.1561 ((x)) _.1563))))
              ;(disj
               ;(pause
                ;(state ((lambda (app (lambda _.198) _.195))))
                ;(conj (== (var _.344) _.195) (lookupo _.344 ((x)) _.196)))
               ;(pause
                ;(state ((lambda (app (lambda _.198) _.195))))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.345 _.346) _.195)
                    ;(eval-expo _.345 ((x)) (closure _.349 _.348)))
                   ;(eval-expo _.346 ((x)) _.347))
                  ;(eval-expo _.349 (_.347 . _.348) _.196))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.350 _.351) _.195)
                     ;(== (_.352 . _.353) _.196))
                    ;(eval-expo _.350 ((x)) _.352))
                   ;(eval-expo _.351 ((x)) _.353))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.354) _.195) (== _.355 _.196))
                    ;(eval-expo _.354 ((x)) (_.355 . _.356)))
                   ;(conj
                    ;(conj (== (cdr _.357) _.195) (== _.359 _.196))
                    ;(eval-expo _.357 ((x)) (_.358 . _.359)))))))))
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (app '(closure _.198 _.197) _.195))))
                ;(conj (== '_.422 _.195) (== _.422 _.196)))
               ;(pause
                ;(state ((lambda (app '(closure _.198 _.197) _.195))))
                ;(disj
                 ;(conj
                  ;(== (list . _.423) _.195)
                  ;(eval-listo _.423 ((x)) _.196))
                 ;(disj
                  ;(conj (== (var _.424) _.195) (lookupo _.424 ((x)) _.196))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.425 _.426) _.195)
                      ;(eval-expo _.425 ((x)) (closure _.429 _.428)))
                     ;(eval-expo _.426 ((x)) _.427))
                    ;(eval-expo _.429 (_.427 . _.428) _.196))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.430 _.431) _.195)
                       ;(== (_.432 . _.433) _.196))
                      ;(eval-expo _.430 ((x)) _.432))
                     ;(eval-expo _.431 ((x)) _.433))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.434) _.195) (== _.435 _.196))
                      ;(eval-expo _.434 ((x)) (_.435 . _.436)))
                     ;(conj
                      ;(conj (== (cdr _.437) _.195) (== _.439 _.196))
                      ;(eval-expo _.437 ((x)) (_.438 . _.439))))))))))
              ;(conj
               ;(disj
                ;(disj
                 ;(pause
                  ;(state ((lambda (app (list . _.298) _.195))))
                  ;(conj (== () _.298) (== () (closure _.198 _.197))))
                 ;(pause
                  ;(state ((lambda (app (list . _.298) _.195))))
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (_.1339 . _.1340) _.298)
                     ;(== (_.1341 . _.1342) (closure _.198 _.197)))
                    ;(eval-expo _.1339 ((x)) _.1341))
                   ;(eval-listo _.1340 ((x)) _.1342))))
                ;(disj
                 ;(pause
                  ;(state ((lambda (app _.194 _.195))))
                  ;(conj
                   ;(== (var _.299) _.194)
                   ;(lookupo _.299 ((x)) (closure _.198 _.197))))
                 ;(pause
                  ;(state ((lambda (app _.194 _.195))))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.300 _.301) _.194)
                      ;(eval-expo _.300 ((x)) (closure _.304 _.303)))
                     ;(eval-expo _.301 ((x)) _.302))
                    ;(eval-expo _.304 (_.302 . _.303) (closure _.198 _.197)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.305 _.306) _.194)
                       ;(== (_.307 . _.308) (closure _.198 _.197)))
                      ;(eval-expo _.305 ((x)) _.307))
                     ;(eval-expo _.306 ((x)) _.308))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.309) _.194)
                       ;(== _.310 (closure _.198 _.197)))
                      ;(eval-expo _.309 ((x)) (_.310 . _.311)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.312) _.194)
                       ;(== _.314 (closure _.198 _.197)))
                      ;(eval-expo _.312 ((x)) (_.313 . _.314)))))))))
               ;(eval-expo _.195 (_.139 . _.140) _.196))))
            ;(eval-expo _.198 (_.196 . _.197) _.131))
           ;(disj
            ;(disj
             ;(pause
              ;(state
               ;((lambda (app '(closure (list . _.695) _.197) (lambda _.421)))))
              ;(conj (== () _.695) (== () (1 x))))
             ;(pause
              ;(state
               ;((lambda (app '(closure (list . _.695) _.197) (lambda _.421)))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (_.2592 . _.2593) _.695)
                 ;(== (_.2594 . _.2595) (1 x)))
                ;(eval-expo _.2592 ((closure _.421 ((x))) . _.197) _.2594))
               ;(eval-listo _.2593 ((closure _.421 ((x))) . _.197) _.2595))))
            ;(disj
             ;(pause
              ;(state ((lambda (app '(closure _.198 _.197) (lambda _.421)))))
              ;(conj
               ;(== (var _.696) _.198)
               ;(lookupo _.696 ((closure _.421 ((x))) . _.197) (1 x))))
             ;(pause
              ;(state ((lambda (app '(closure _.198 _.197) (lambda _.421)))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.697 _.698) _.198)
                  ;(eval-expo
                   ;_.697
                   ;((closure _.421 ((x))) . _.197)
                   ;(closure _.701 _.700)))
                 ;(eval-expo _.698 ((closure _.421 ((x))) . _.197) _.699))
                ;(eval-expo _.701 (_.699 . _.700) (1 x)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.702 _.703) _.198)
                   ;(== (_.704 . _.705) (1 x)))
                  ;(eval-expo _.702 ((closure _.421 ((x))) . _.197) _.704))
                 ;(eval-expo _.703 ((closure _.421 ((x))) . _.197) _.705))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.706) _.198) (== _.707 (1 x)))
                  ;(eval-expo
                   ;_.706
                   ;((closure _.421 ((x))) . _.197)
                   ;(_.707 . _.708)))
                 ;(conj
                  ;(conj (== (cdr _.709) _.198) (== _.711 (1 x)))
                  ;(eval-expo
                   ;_.709
                   ;((closure _.421 ((x))) . _.197)
                   ;(_.710 . _.711))))))))))
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (app (lambda _.198) '_.196))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.618 _.619) _.198)
                 ;(eval-expo _.618 (_.196 (x)) (closure _.622 _.621)))
                ;(eval-expo _.619 (_.196 (x)) _.620))
               ;(eval-expo _.622 (_.620 . _.621) (1 x))))
             ;(pause
              ;(state ((lambda (app (lambda _.198) '_.196))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.623 _.624) _.198)
                  ;(== (_.625 . _.626) (1 x)))
                 ;(eval-expo _.623 (_.196 (x)) _.625))
                ;(eval-expo _.624 (_.196 (x)) _.626))
               ;(disj
                ;(conj
                 ;(conj (== (car _.627) _.198) (== _.628 (1 x)))
                 ;(eval-expo _.627 (_.196 (x)) (_.628 . _.629)))
                ;(conj
                 ;(conj (== (cdr _.630) _.198) (== _.632 (1 x)))
                 ;(eval-expo _.630 (_.196 (x)) (_.631 . _.632)))))))
            ;(pause
             ;(state ((lambda (app (lambda (var _.617)) '_.196))))
             ;(conj (== (s . _.1998) _.617) (lookupo _.1998 ((x)) (1 x)))))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (app (lambda (list _.1499 . _.1500)) '_.196))))
              ;(conj (== '_.2316 _.1499) (== _.2316 1)))
             ;(pause
              ;(state ((lambda (app (lambda (list _.1499 . _.1500)) '_.196))))
              ;(disj
               ;(conj
                ;(== (list . _.2317) _.1499)
                ;(eval-listo _.2317 (_.196 (x)) 1))
               ;(disj
                ;(conj (== (var _.2318) _.1499) (lookupo _.2318 (_.196 (x)) 1))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.2319 _.2320) _.1499)
                    ;(eval-expo _.2319 (_.196 (x)) (closure _.2323 _.2322)))
                   ;(eval-expo _.2320 (_.196 (x)) _.2321))
                  ;(eval-expo _.2323 (_.2321 . _.2322) 1))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.2324 _.2325) _.1499)
                     ;(== (_.2326 . _.2327) 1))
                    ;(eval-expo _.2324 (_.196 (x)) _.2326))
                   ;(eval-expo _.2325 (_.196 (x)) _.2327))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.2328) _.1499) (== _.2329 1))
                    ;(eval-expo _.2328 (_.196 (x)) (_.2329 . _.2330)))
                   ;(conj
                    ;(conj (== (cdr _.2331) _.1499) (== _.2333 1))
                    ;(eval-expo _.2331 (_.196 (x)) (_.2332 . _.2333))))))))))
            ;(eval-listo _.1500 (_.196 . _.197) _.1502)))))))
      ;(eval-listo _.130 () _.132))
     ;(conj
      ;(disj
       ;(conj
        ;(disj
         ;(disj
          ;(pause
           ;(state ((lambda (app (lambda (var ())) '(1 x)))))
           ;(conj (== (lambda _.3048) '(y)) (== (closure _.3048 ()) _.2873)))
          ;(pause
           ;(state ((lambda (app (lambda (var ())) '(1 x)))))
           ;(disj
            ;(conj (== '_.3049 '(y)) (== _.3049 _.2873))
            ;(disj
             ;(conj (== (list . _.3050) '(y)) (eval-listo _.3050 () _.2873))
             ;(disj
              ;(conj (== (var _.3051) '(y)) (lookupo _.3051 () _.2873))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.3052 _.3053) '(y))
                  ;(eval-expo _.3052 () (closure _.3056 _.3055)))
                 ;(eval-expo _.3053 () _.3054))
                ;(eval-expo _.3056 (_.3054 . _.3055) _.2873))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.3057 _.3058) '(y))
                   ;(== (_.3059 . _.3060) _.2873))
                  ;(eval-expo _.3057 () _.3059))
                 ;(eval-expo _.3058 () _.3060))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.3061) '(y)) (== _.3062 _.2873))
                  ;(eval-expo _.3061 () (_.3062 . _.3063)))
                 ;(conj
                  ;(conj (== (cdr _.3064) '(y)) (== _.3066 _.2873))
                  ;(eval-expo _.3064 () (_.3065 . _.3066)))))))))))
         ;(conj
          ;(disj
           ;(pause
            ;(state ((lambda (app (lambda (var ())) '(1 x)))))
            ;(conj
             ;(== '_.3027 (lambda (app (lambda (var ())) '(1 x))))
             ;(== _.3027 (closure _.2875 _.2874))))
           ;(pause
            ;(state ((lambda (app (lambda (var ())) '(1 x)))))
            ;(disj
             ;(conj
              ;(== (list . _.3028) (lambda (app (lambda (var ())) '(1 x))))
              ;(eval-listo _.3028 () (closure _.2875 _.2874)))
             ;(disj
              ;(conj
               ;(== (var _.3029) (lambda (app (lambda (var ())) '(1 x))))
               ;(lookupo _.3029 () (closure _.2875 _.2874)))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(==
                   ;(app _.3030 _.3031)
                   ;(lambda (app (lambda (var ())) '(1 x))))
                  ;(eval-expo _.3030 () (closure _.3034 _.3033)))
                 ;(eval-expo _.3031 () _.3032))
                ;(eval-expo _.3034 (_.3032 . _.3033) (closure _.2875 _.2874)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(==
                    ;(cons _.3035 _.3036)
                    ;(lambda (app (lambda (var ())) '(1 x))))
                   ;(== (_.3037 . _.3038) (closure _.2875 _.2874)))
                  ;(eval-expo _.3035 () _.3037))
                 ;(eval-expo _.3036 () _.3038))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(== (car _.3039) (lambda (app (lambda (var ())) '(1 x))))
                   ;(== _.3040 (closure _.2875 _.2874)))
                  ;(eval-expo _.3039 () (_.3040 . _.3041)))
                 ;(conj
                  ;(conj
                   ;(== (cdr _.3042) (lambda (app (lambda (var ())) '(1 x))))
                   ;(== _.3044 (closure _.2875 _.2874)))
                  ;(eval-expo _.3042 () (_.3043 . _.3044))))))))))
          ;(eval-expo _.2872 () _.2873)))
        ;(eval-expo _.2875 (_.2873 . _.2874) _.2827))
       ;(disj
        ;(pause
         ;(state ((lambda (app (lambda (var ())) '(1 x)))))
         ;(conj
          ;(conj
           ;(== (car _.2880) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           ;(== _.2881 (1 y)))
          ;(eval-expo _.2880 () (_.2881 . _.2882))))
        ;(pause
         ;(state ((lambda (app (lambda (var ())) '(1 x)))))
         ;(conj
          ;(conj
           ;(== (cdr _.2883) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           ;(== _.2885 (1 y)))
          ;(eval-expo _.2883 () (_.2884 . _.2885))))))
      ;(eval-listo _.2826 () _.2828)))
    ;(conj
     ;(disj
      ;(conj
       ;(disj
        ;(conj
         ;(disj
          ;(pause
           ;(state ((lambda (list (car '(1 . _.273)) 'x))))
           ;(conj
            ;(conj
             ;(conj
              ;(== (app _.2890 _.2891) (lambda (list (car '(1 . _.273)) 'x)))
              ;(eval-expo _.2890 () (closure _.2894 _.2893)))
             ;(eval-expo _.2891 () _.2892))
            ;(eval-expo _.2894 (_.2892 . _.2893) (closure _.2757 _.2756))))
          ;(pause
           ;(state ((lambda (list (car '(1 . _.273)) 'x))))
           ;(disj
            ;(conj
             ;(conj
              ;(conj
               ;(== (cons _.2895 _.2896) (lambda (list (car '(1 . _.273)) 'x)))
               ;(== (_.2897 . _.2898) (closure _.2757 _.2756)))
              ;(eval-expo _.2895 () _.2897))
             ;(eval-expo _.2896 () _.2898))
            ;(disj
             ;(conj
              ;(conj
               ;(== (car _.2899) (lambda (list (car '(1 . _.273)) 'x)))
               ;(== _.2900 (closure _.2757 _.2756)))
              ;(eval-expo _.2899 () (_.2900 . _.2901)))
             ;(conj
              ;(conj
               ;(== (cdr _.2902) (lambda (list (car '(1 . _.273)) 'x)))
               ;(== _.2904 (closure _.2757 _.2756)))
              ;(eval-expo _.2902 () (_.2903 . _.2904)))))))
         ;(eval-expo _.2754 () _.2755))
        ;(pause
         ;(state ((lambda (list (car '(1 . _.273)) 'x))))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (app _.2909 _.2910) '(y))
             ;(eval-expo _.2909 () (closure _.2913 _.2912)))
            ;(eval-expo _.2910 () _.2911))
           ;(eval-expo _.2913 (_.2911 . _.2912) _.2755))
          ;(disj
           ;(conj
            ;(conj
             ;(conj
              ;(== (cons _.2914 _.2915) '(y))
              ;(== (_.2916 . _.2917) _.2755))
             ;(eval-expo _.2914 () _.2916))
            ;(eval-expo _.2915 () _.2917))
           ;(disj
            ;(conj
             ;(conj (== (car _.2918) '(y)) (== _.2919 _.2755))
             ;(eval-expo _.2918 () (_.2919 . _.2920)))
            ;(conj
             ;(conj (== (cdr _.2921) '(y)) (== _.2923 _.2755))
             ;(eval-expo _.2921 () (_.2922 . _.2923))))))))
       ;(eval-expo _.2757 (_.2755 . _.2756) _.2743))
      ;(disj
       ;(pause
        ;(state ((lambda (list (car '(1 . _.273)) 'x))))
        ;(conj
         ;(conj
          ;(conj
           ;(== (_.3067 . _.3068) ((car '(1 . _.273)) 'x))
           ;(== (_.3069 . _.3070) (1 y)))
          ;(eval-expo _.3067 ((y)) _.3069))
         ;(eval-listo _.3068 ((y)) _.3070)))
       ;(pause
        ;(state ((lambda (list (car '(1 . _.273)) 'x))))
        ;(disj
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.2966 _.2967) (list (car '(1 . _.273)) 'x))
            ;(eval-expo _.2966 ((y)) (closure _.2970 _.2969)))
           ;(eval-expo _.2967 ((y)) _.2968))
          ;(eval-expo _.2970 (_.2968 . _.2969) (1 y)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (cons _.2971 _.2972) (list (car '(1 . _.273)) 'x))
             ;(== (_.2973 . _.2974) (1 y)))
            ;(eval-expo _.2971 ((y)) _.2973))
           ;(eval-expo _.2972 ((y)) _.2974))
          ;(disj
           ;(conj
            ;(conj
             ;(== (car _.2975) (list (car '(1 . _.273)) 'x))
             ;(== _.2976 (1 y)))
            ;(eval-expo _.2975 ((y)) (_.2976 . _.2977)))
           ;(conj
            ;(conj
             ;(== (cdr _.2978) (list (car '(1 . _.273)) 'x))
             ;(== _.2980 (1 y)))
            ;(eval-expo _.2978 ((y)) (_.2979 . _.2980)))))))))
     ;(eval-listo _.2742 () _.2744)))))


;;; (stream-pretty (step 1608 q-1p))
;;; Snapshot containing the first answer: (lambda (cons '1 (var ())))

;'((((lambda (cons '1 (var ())))))
  ;(disj
   ;(disj
    ;(disj
     ;(conj
      ;(disj
       ;(disj
        ;(disj
         ;(disj
          ;(disj
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state ((lambda (list (cdr _.3226) . _.3185))))
               ;(disj
                ;(conj
                 ;(== (list . _.3879) _.3226)
                 ;(eval-listo _.3879 ((x)) (_.3227 . 1)))
                ;(disj
                 ;(conj
                  ;(== (var _.3880) _.3226)
                  ;(lookupo _.3880 ((x)) (_.3227 . 1)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.3881 _.3882) _.3226)
                     ;(eval-expo _.3881 ((x)) (closure _.3885 _.3884)))
                    ;(eval-expo _.3882 ((x)) _.3883))
                   ;(eval-expo _.3885 (_.3883 . _.3884) (_.3227 . 1)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.3886 _.3887) _.3226)
                      ;(== (_.3888 . _.3889) (_.3227 . 1)))
                     ;(eval-expo _.3886 ((x)) _.3888))
                    ;(eval-expo _.3887 ((x)) _.3889))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.3890) _.3226) (== _.3891 (_.3227 . 1)))
                     ;(eval-expo _.3890 ((x)) (_.3891 . _.3892)))
                    ;(conj
                     ;(conj (== (cdr _.3893) _.3226) (== _.3895 (_.3227 . 1)))
                     ;(eval-expo _.3893 ((x)) (_.3894 . _.3895)))))))))
              ;(disj
               ;(pause
                ;(state ((lambda (list (car _.3223) . _.3185))))
                ;(conj
                 ;(== (list . _.3777) _.3223)
                 ;(eval-listo _.3777 ((x)) (1 . _.3225))))
               ;(pause
                ;(state ((lambda (list (car _.3223) . _.3185))))
                ;(disj
                 ;(conj
                  ;(== (var _.3778) _.3223)
                  ;(lookupo _.3778 ((x)) (1 . _.3225)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.3779 _.3780) _.3223)
                     ;(eval-expo _.3779 ((x)) (closure _.3783 _.3782)))
                    ;(eval-expo _.3780 ((x)) _.3781))
                   ;(eval-expo _.3783 (_.3781 . _.3782) (1 . _.3225)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.3784 _.3785) _.3223)
                      ;(== (_.3786 . _.3787) (1 . _.3225)))
                     ;(eval-expo _.3784 ((x)) _.3786))
                    ;(eval-expo _.3785 ((x)) _.3787))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.3788) _.3223) (== _.3789 (1 . _.3225)))
                     ;(eval-expo _.3788 ((x)) (_.3789 . _.3790)))
                    ;(conj
                     ;(conj (== (cdr _.3791) _.3223) (== _.3793 (1 . _.3225)))
                     ;(eval-expo _.3791 ((x)) (_.3792 . _.3793))))))))))
             ;(disj
              ;(disj
               ;(pause
                ;(state
                 ;((lambda (list
                           ;(app (lambda _.3218) (lambda _.3604))
                           ;.
                           ;_.3185))))
                ;(conj
                 ;(== (list . _.3739) _.3218)
                 ;(eval-listo _.3739 ((closure _.3604 ((x))) (x)) 1)))
               ;(pause
                ;(state
                 ;((lambda (list
                           ;(app (lambda _.3218) (lambda _.3604))
                           ;.
                           ;_.3185))))
                ;(disj
                 ;(conj
                  ;(== (var _.3740) _.3218)
                  ;(lookupo _.3740 ((closure _.3604 ((x))) (x)) 1))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.3741 _.3742) _.3218)
                     ;(eval-expo
                      ;_.3741
                      ;((closure _.3604 ((x))) (x))
                      ;(closure _.3745 _.3744)))
                    ;(eval-expo _.3742 ((closure _.3604 ((x))) (x)) _.3743))
                   ;(eval-expo _.3745 (_.3743 . _.3744) 1))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.3746 _.3747) _.3218)
                      ;(== (_.3748 . _.3749) 1))
                     ;(eval-expo _.3746 ((closure _.3604 ((x))) (x)) _.3748))
                    ;(eval-expo _.3747 ((closure _.3604 ((x))) (x)) _.3749))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.3750) _.3218) (== _.3751 1))
                     ;(eval-expo
                      ;_.3750
                      ;((closure _.3604 ((x))) (x))
                      ;(_.3751 . _.3752)))
                    ;(conj
                     ;(conj (== (cdr _.3753) _.3218) (== _.3755 1))
                     ;(eval-expo
                      ;_.3753
                      ;((closure _.3604 ((x))) (x))
                      ;(_.3754 . _.3755)))))))))
              ;(disj
               ;(disj
                ;(pause
                 ;(state
                  ;((lambda (list (app (lambda _.3218) '_.3216) . _.3185))))
                 ;(conj
                  ;(== (lambda _.4455) _.3218)
                  ;(== (closure _.4455 (_.3216 (x))) 1)))
                ;(pause
                 ;(state
                  ;((lambda (list (app (lambda _.3218) '_.3216) . _.3185))))
                 ;(disj
                  ;(conj (== '_.4456 _.3218) (== _.4456 1))
                  ;(disj
                   ;(conj
                    ;(== (list . _.4457) _.3218)
                    ;(eval-listo _.4457 (_.3216 (x)) 1))
                   ;(disj
                    ;(conj
                     ;(== (var _.4458) _.3218)
                     ;(lookupo _.4458 (_.3216 (x)) 1))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.4459 _.4460) _.3218)
                        ;(eval-expo
                         ;_.4459
                         ;(_.3216 (x))
                         ;(closure _.4463 _.4462)))
                       ;(eval-expo _.4460 (_.3216 (x)) _.4461))
                      ;(eval-expo _.4463 (_.4461 . _.4462) 1))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.4464 _.4465) _.3218)
                         ;(== (_.4466 . _.4467) 1))
                        ;(eval-expo _.4464 (_.3216 (x)) _.4466))
                       ;(eval-expo _.4465 (_.3216 (x)) _.4467))
                      ;(disj
                       ;(conj
                        ;(conj (== (car _.4468) _.3218) (== _.4469 1))
                        ;(eval-expo _.4468 (_.3216 (x)) (_.4469 . _.4470)))
                       ;(conj
                        ;(conj (== (cdr _.4471) _.3218) (== _.4473 1))
                        ;(eval-expo
                         ;_.4471
                         ;(_.3216 (x))
                         ;(_.4472 . _.4473)))))))))))
               ;(disj
                ;(conj
                 ;(disj
                  ;(pause
                   ;(state
                    ;((lambda (list (app (lambda _.3218) _.3215) . _.3185))))
                   ;(disj
                    ;(conj
                     ;(== (list . _.3606) _.3215)
                     ;(eval-listo _.3606 ((x)) _.3216))
                    ;(disj
                     ;(conj
                      ;(== (var _.3607) _.3215)
                      ;(lookupo _.3607 ((x)) _.3216))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (app _.3608 _.3609) _.3215)
                         ;(eval-expo _.3608 ((x)) (closure _.3612 _.3611)))
                        ;(eval-expo _.3609 ((x)) _.3610))
                       ;(eval-expo _.3612 (_.3610 . _.3611) _.3216))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(conj
                          ;(== (cons _.3613 _.3614) _.3215)
                          ;(== (_.3615 . _.3616) _.3216))
                         ;(eval-expo _.3613 ((x)) _.3615))
                        ;(eval-expo _.3614 ((x)) _.3616))
                       ;(disj
                        ;(conj
                         ;(conj (== (car _.3617) _.3215) (== _.3618 _.3216))
                         ;(eval-expo _.3617 ((x)) (_.3618 . _.3619)))
                        ;(conj
                         ;(conj (== (cdr _.3620) _.3215) (== _.3622 _.3216))
                         ;(eval-expo _.3620 ((x)) (_.3621 . _.3622)))))))))
                  ;(disj
                   ;(conj
                    ;(disj
                     ;(pause
                      ;(state ((lambda (list (app _.3214 _.3215) . _.3185))))
                      ;(conj
                       ;(== (list . _.3485) _.3214)
                       ;(eval-listo _.3485 ((x)) (closure _.3218 _.3217))))
                     ;(pause
                      ;(state ((lambda (list (app _.3214 _.3215) . _.3185))))
                      ;(disj
                       ;(conj
                        ;(== (var _.3486) _.3214)
                        ;(lookupo _.3486 ((x)) (closure _.3218 _.3217)))
                       ;(disj
                        ;(conj
                         ;(conj
                          ;(conj
                           ;(== (app _.3487 _.3488) _.3214)
                           ;(eval-expo _.3487 ((x)) (closure _.3491 _.3490)))
                          ;(eval-expo _.3488 ((x)) _.3489))
                         ;(eval-expo
                          ;_.3491
                          ;(_.3489 . _.3490)
                          ;(closure _.3218 _.3217)))
                        ;(disj
                         ;(conj
                          ;(conj
                           ;(conj
                            ;(== (cons _.3492 _.3493) _.3214)
                            ;(== (_.3494 . _.3495) (closure _.3218 _.3217)))
                           ;(eval-expo _.3492 ((x)) _.3494))
                          ;(eval-expo _.3493 ((x)) _.3495))
                         ;(disj
                          ;(conj
                           ;(conj
                            ;(== (car _.3496) _.3214)
                            ;(== _.3497 (closure _.3218 _.3217)))
                           ;(eval-expo _.3496 ((x)) (_.3497 . _.3498)))
                          ;(conj
                           ;(conj
                            ;(== (cdr _.3499) _.3214)
                            ;(== _.3501 (closure _.3218 _.3217)))
                           ;(eval-expo _.3499 ((x)) (_.3500 . _.3501)))))))))
                    ;(eval-expo _.3215 (_.3091 . _.3092) _.3216))
                   ;(pause
                    ;(state
                     ;((lambda (list
                               ;(app '(closure _.3218 _.3217) _.3215)
                               ;.
                               ;_.3185))))
                    ;(disj
                     ;(conj (== '_.3840 _.3215) (== _.3840 _.3216))
                     ;(disj
                      ;(conj
                       ;(== (list . _.3841) _.3215)
                       ;(eval-listo _.3841 ((x)) _.3216))
                      ;(disj
                       ;(conj
                        ;(== (var _.3842) _.3215)
                        ;(lookupo _.3842 ((x)) _.3216))
                       ;(disj
                        ;(conj
                         ;(conj
                          ;(conj
                           ;(== (app _.3843 _.3844) _.3215)
                           ;(eval-expo _.3843 ((x)) (closure _.3847 _.3846)))
                          ;(eval-expo _.3844 ((x)) _.3845))
                         ;(eval-expo _.3847 (_.3845 . _.3846) _.3216))
                        ;(disj
                         ;(conj
                          ;(conj
                           ;(conj
                            ;(== (cons _.3848 _.3849) _.3215)
                            ;(== (_.3850 . _.3851) _.3216))
                           ;(eval-expo _.3848 ((x)) _.3850))
                          ;(eval-expo _.3849 ((x)) _.3851))
                         ;(disj
                          ;(conj
                           ;(conj (== (car _.3852) _.3215) (== _.3853 _.3216))
                           ;(eval-expo _.3852 ((x)) (_.3853 . _.3854)))
                          ;(conj
                           ;(conj (== (cdr _.3855) _.3215) (== _.3857 _.3216))
                           ;(eval-expo _.3855 ((x)) (_.3856 . _.3857))))))))))))
                 ;(eval-expo _.3218 (_.3216 . _.3217) _.3186))
                ;(disj
                 ;(pause
                  ;(state
                   ;((lambda (list
                             ;(app '(closure _.3218 _.3217) (lambda _.3839))
                             ;.
                             ;_.3185))))
                  ;(conj
                   ;(== (lambda _.5632) _.3218)
                   ;(== (closure _.5632 ((closure _.3839 ((x))) . _.3217)) 1)))
                 ;(pause
                  ;(state
                   ;((lambda (list
                             ;(app '(closure _.3218 _.3217) (lambda _.3839))
                             ;.
                             ;_.3185))))
                  ;(disj
                   ;(conj (== '_.5633 _.3218) (== _.5633 1))
                   ;(disj
                    ;(conj
                     ;(== (list . _.5634) _.3218)
                     ;(eval-listo _.5634 ((closure _.3839 ((x))) . _.3217) 1))
                    ;(disj
                     ;(conj
                      ;(== (var _.5635) _.3218)
                      ;(lookupo _.5635 ((closure _.3839 ((x))) . _.3217) 1))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (app _.5636 _.5637) _.3218)
                         ;(eval-expo
                          ;_.5636
                          ;((closure _.3839 ((x))) . _.3217)
                          ;(closure _.5640 _.5639)))
                        ;(eval-expo
                         ;_.5637
                         ;((closure _.3839 ((x))) . _.3217)
                         ;_.5638))
                       ;(eval-expo _.5640 (_.5638 . _.5639) 1))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(conj
                          ;(== (cons _.5641 _.5642) _.3218)
                          ;(== (_.5643 . _.5644) 1))
                         ;(eval-expo
                          ;_.5641
                          ;((closure _.3839 ((x))) . _.3217)
                          ;_.5643))
                        ;(eval-expo
                         ;_.5642
                         ;((closure _.3839 ((x))) . _.3217)
                         ;_.5644))
                       ;(disj
                        ;(conj
                         ;(conj (== (car _.5645) _.3218) (== _.5646 1))
                         ;(eval-expo
                          ;_.5645
                          ;((closure _.3839 ((x))) . _.3217)
                          ;(_.5646 . _.5647)))
                        ;(conj
                         ;(conj (== (cdr _.5648) _.3218) (== _.5650 1))
                         ;(eval-expo
                          ;_.5648
                          ;((closure _.3839 ((x))) . _.3217)
                          ;(_.5649 . _.5650)))))))))))))))
            ;(eval-listo _.3185 (_.3091 . _.3092) _.3187))
           ;(conj
            ;(pause
             ;(state ((lambda (list (cdr '(_.3227 . 1)) _.4558 . _.4559))))
             ;(disj
              ;(conj (== '_.5474 _.4558) (== _.5474 x))
              ;(disj
               ;(conj (== (list . _.5475) _.4558) (eval-listo _.5475 ((x)) x))
               ;(disj
                ;(conj (== (var _.5476) _.4558) (lookupo _.5476 ((x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.5477 _.5478) _.4558)
                    ;(eval-expo _.5477 ((x)) (closure _.5481 _.5480)))
                   ;(eval-expo _.5478 ((x)) _.5479))
                  ;(eval-expo _.5481 (_.5479 . _.5480) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.5482 _.5483) _.4558)
                     ;(== (_.5484 . _.5485) x))
                    ;(eval-expo _.5482 ((x)) _.5484))
                   ;(eval-expo _.5483 ((x)) _.5485))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.5486) _.4558) (== _.5487 x))
                    ;(eval-expo _.5486 ((x)) (_.5487 . _.5488)))
                   ;(conj
                    ;(conj (== (cdr _.5489) _.4558) (== _.5491 x))
                    ;(eval-expo _.5489 ((x)) (_.5490 . _.5491))))))))))
            ;(eval-listo _.4559 (_.3091 . _.3092) _.4561)))
          ;(disj
           ;(pause
            ;(state ((lambda (list (car '(1 . _.3225)) 'x . _.4448))))
            ;(conj
             ;(conj
              ;(conj (== (_.5423 . _.5424) _.4448) (== (_.5425 . _.5426) ()))
              ;(eval-expo _.5423 ((x)) _.5425))
             ;(eval-listo _.5424 ((x)) _.5426)))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (list (car '(1 . _.3225)) _.4447 . _.4448))))
              ;(disj
               ;(conj (== (var _.4580) _.4447) (lookupo _.4580 ((x)) x))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (app _.4581 _.4582) _.4447)
                   ;(eval-expo _.4581 ((x)) (closure _.4585 _.4584)))
                  ;(eval-expo _.4582 ((x)) _.4583))
                 ;(eval-expo _.4585 (_.4583 . _.4584) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (cons _.4586 _.4587) _.4447)
                    ;(== (_.4588 . _.4589) x))
                   ;(eval-expo _.4586 ((x)) _.4588))
                  ;(eval-expo _.4587 ((x)) _.4589))
                 ;(disj
                  ;(conj
                   ;(conj (== (car _.4590) _.4447) (== _.4591 x))
                   ;(eval-expo _.4590 ((x)) (_.4591 . _.4592)))
                  ;(conj
                   ;(conj (== (cdr _.4593) _.4447) (== _.4595 x))
                   ;(eval-expo _.4593 ((x)) (_.4594 . _.4595))))))))
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list (car '(1 . _.3225)) (list . _.4579) . _.4448))))
               ;(conj (== () _.4579) (== () x)))
              ;(pause
               ;(state
                ;((lambda (list (car '(1 . _.3225)) (list . _.4579) . _.4448))))
               ;(conj
                ;(conj
                 ;(conj (== (_.5933 . _.5934) _.4579) (== (_.5935 . _.5936) x))
                 ;(eval-expo _.5933 ((x)) _.5935))
                ;(eval-listo _.5934 ((x)) _.5936)))))
            ;(eval-listo _.4448 (_.3091 . _.3092) _.4450))))
         ;(conj
          ;(disj
           ;(pause
            ;(state
             ;((lambda (list
                       ;(app (lambda '1) (lambda _.3604))
                       ;_.4401
                       ;.
                       ;_.4402))))
            ;(disj
             ;(conj
              ;(conj
               ;(conj (== (cons _.4502 _.4503) _.4401) (== (_.4504 . _.4505) x))
               ;(eval-expo _.4502 ((x)) _.4504))
              ;(eval-expo _.4503 ((x)) _.4505))
             ;(disj
              ;(conj
               ;(conj (== (car _.4506) _.4401) (== _.4507 x))
               ;(eval-expo _.4506 ((x)) (_.4507 . _.4508)))
              ;(conj
               ;(conj (== (cdr _.4509) _.4401) (== _.4511 x))
               ;(eval-expo _.4509 ((x)) (_.4510 . _.4511))))))
           ;(conj
            ;(conj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;(app (lambda '1) (lambda _.3604))
                          ;(app _.4497 _.4498)
                          ;.
                          ;_.4402))))
               ;(conj
                ;(== (lambda _.5876) _.4497)
                ;(== (closure _.5876 ((x))) (closure _.4501 _.4500))))
              ;(pause
               ;(state
                ;((lambda (list
                          ;(app (lambda '1) (lambda _.3604))
                          ;(app _.4497 _.4498)
                          ;.
                          ;_.4402))))
               ;(disj
                ;(conj (== '_.5877 _.4497) (== _.5877 (closure _.4501 _.4500)))
                ;(disj
                 ;(conj
                  ;(== (list . _.5878) _.4497)
                  ;(eval-listo _.5878 ((x)) (closure _.4501 _.4500)))
                 ;(disj
                  ;(conj
                   ;(== (var _.5879) _.4497)
                   ;(lookupo _.5879 ((x)) (closure _.4501 _.4500)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.5880 _.5881) _.4497)
                      ;(eval-expo _.5880 ((x)) (closure _.5884 _.5883)))
                     ;(eval-expo _.5881 ((x)) _.5882))
                    ;(eval-expo
                     ;_.5884
                     ;(_.5882 . _.5883)
                     ;(closure _.4501 _.4500)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.5885 _.5886) _.4497)
                       ;(== (_.5887 . _.5888) (closure _.4501 _.4500)))
                      ;(eval-expo _.5885 ((x)) _.5887))
                     ;(eval-expo _.5886 ((x)) _.5888))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.5889) _.4497)
                       ;(== _.5890 (closure _.4501 _.4500)))
                      ;(eval-expo _.5889 ((x)) (_.5890 . _.5891)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.5892) _.4497)
                       ;(== _.5894 (closure _.4501 _.4500)))
                      ;(eval-expo _.5892 ((x)) (_.5893 . _.5894)))))))))))
             ;(eval-expo _.4498 (_.3091 . _.3092) _.4499))
            ;(eval-expo _.4501 (_.4499 . _.4500) _.4403)))
          ;(eval-listo _.4402 (_.3091 . _.3092) _.4404)))
        ;(conj
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app '(closure _.3343 _.3342) (lambda _.4299))
                          ;.
                          ;_.3287))))
               ;(conj
                ;(== (lambda _.5324) _.3343)
                ;(== (closure _.5324 ((closure _.4299 ((x))) . _.3342)) x)))
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app '(closure _.3343 _.3342) (lambda _.4299))
                          ;.
                          ;_.3287))))
               ;(disj
                ;(conj (== '_.5325 _.3343) (== _.5325 x))
                ;(disj
                 ;(conj
                  ;(== (list . _.5326) _.3343)
                  ;(eval-listo _.5326 ((closure _.4299 ((x))) . _.3342) x))
                 ;(disj
                  ;(conj
                   ;(== (var _.5327) _.3343)
                   ;(lookupo _.5327 ((closure _.4299 ((x))) . _.3342) x))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.5328 _.5329) _.3343)
                      ;(eval-expo
                       ;_.5328
                       ;((closure _.4299 ((x))) . _.3342)
                       ;(closure _.5332 _.5331)))
                     ;(eval-expo
                      ;_.5329
                      ;((closure _.4299 ((x))) . _.3342)
                      ;_.5330))
                    ;(eval-expo _.5332 (_.5330 . _.5331) x))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.5333 _.5334) _.3343)
                       ;(== (_.5335 . _.5336) x))
                      ;(eval-expo
                       ;_.5333
                       ;((closure _.4299 ((x))) . _.3342)
                       ;_.5335))
                     ;(eval-expo
                      ;_.5334
                      ;((closure _.4299 ((x))) . _.3342)
                      ;_.5336))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.5337) _.3343) (== _.5338 x))
                      ;(eval-expo
                       ;_.5337
                       ;((closure _.4299 ((x))) . _.3342)
                       ;(_.5338 . _.5339)))
                     ;(conj
                      ;(conj (== (cdr _.5340) _.3343) (== _.5342 x))
                      ;(eval-expo
                       ;_.5340
                       ;((closure _.4299 ((x))) . _.3342)
                       ;(_.5341 . _.5342)))))))))))
             ;(conj
              ;(disj
               ;(disj
                ;(conj
                 ;(disj
                  ;(pause
                   ;(state ((lambda (list '1 (app _.3339 _.3340) . _.3287))))
                   ;(conj
                    ;(== (list . _.3758) _.3339)
                    ;(eval-listo _.3758 ((x)) (closure _.3343 _.3342))))
                  ;(pause
                   ;(state ((lambda (list '1 (app _.3339 _.3340) . _.3287))))
                   ;(disj
                    ;(conj
                     ;(== (var _.3759) _.3339)
                     ;(lookupo _.3759 ((x)) (closure _.3343 _.3342)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.3760 _.3761) _.3339)
                        ;(eval-expo _.3760 ((x)) (closure _.3764 _.3763)))
                       ;(eval-expo _.3761 ((x)) _.3762))
                      ;(eval-expo
                       ;_.3764
                       ;(_.3762 . _.3763)
                       ;(closure _.3343 _.3342)))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.3765 _.3766) _.3339)
                         ;(== (_.3767 . _.3768) (closure _.3343 _.3342)))
                        ;(eval-expo _.3765 ((x)) _.3767))
                       ;(eval-expo _.3766 ((x)) _.3768))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(== (car _.3769) _.3339)
                         ;(== _.3770 (closure _.3343 _.3342)))
                        ;(eval-expo _.3769 ((x)) (_.3770 . _.3771)))
                       ;(conj
                        ;(conj
                         ;(== (cdr _.3772) _.3339)
                         ;(== _.3774 (closure _.3343 _.3342)))
                        ;(eval-expo _.3772 ((x)) (_.3773 . _.3774)))))))))
                 ;(eval-expo _.3340 (_.3091 . _.3092) _.3341))
                ;(pause
                 ;(state
                  ;((lambda (list
                            ;'1
                            ;(app '(closure _.3343 _.3342) _.3340)
                            ;.
                            ;_.3287))))
                 ;(disj
                  ;(conj (== '_.4300 _.3340) (== _.4300 _.3341))
                  ;(disj
                   ;(conj
                    ;(== (list . _.4301) _.3340)
                    ;(eval-listo _.4301 ((x)) _.3341))
                   ;(disj
                    ;(conj
                     ;(== (var _.4302) _.3340)
                     ;(lookupo _.4302 ((x)) _.3341))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.4303 _.4304) _.3340)
                        ;(eval-expo _.4303 ((x)) (closure _.4307 _.4306)))
                       ;(eval-expo _.4304 ((x)) _.4305))
                      ;(eval-expo _.4307 (_.4305 . _.4306) _.3341))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.4308 _.4309) _.3340)
                         ;(== (_.4310 . _.4311) _.3341))
                        ;(eval-expo _.4308 ((x)) _.4310))
                       ;(eval-expo _.4309 ((x)) _.4311))
                      ;(disj
                       ;(conj
                        ;(conj (== (car _.4312) _.3340) (== _.4313 _.3341))
                        ;(eval-expo _.4312 ((x)) (_.4313 . _.4314)))
                       ;(conj
                        ;(conj (== (cdr _.4315) _.3340) (== _.4317 _.3341))
                        ;(eval-expo _.4315 ((x)) (_.4316 . _.4317)))))))))))
               ;(disj
                ;(pause
                 ;(state
                  ;((lambda (list '1 (app (lambda _.3343) _.3340) . _.3287))))
                 ;(conj
                  ;(== (list . _.3921) _.3340)
                  ;(eval-listo _.3921 ((x)) _.3341)))
                ;(pause
                 ;(state
                  ;((lambda (list '1 (app (lambda _.3343) _.3340) . _.3287))))
                 ;(disj
                  ;(conj (== (var _.3922) _.3340) (lookupo _.3922 ((x)) _.3341))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.3923 _.3924) _.3340)
                      ;(eval-expo _.3923 ((x)) (closure _.3927 _.3926)))
                     ;(eval-expo _.3924 ((x)) _.3925))
                    ;(eval-expo _.3927 (_.3925 . _.3926) _.3341))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.3928 _.3929) _.3340)
                       ;(== (_.3930 . _.3931) _.3341))
                      ;(eval-expo _.3928 ((x)) _.3930))
                     ;(eval-expo _.3929 ((x)) _.3931))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.3932) _.3340) (== _.3933 _.3341))
                      ;(eval-expo _.3932 ((x)) (_.3933 . _.3934)))
                     ;(conj
                      ;(conj (== (cdr _.3935) _.3340) (== _.3937 _.3341))
                      ;(eval-expo _.3935 ((x)) (_.3936 . _.3937))))))))))
              ;(eval-expo _.3343 (_.3341 . _.3342) _.3288)))
            ;(disj
             ;(pause
              ;(state
               ;((lambda (list '1 (app (lambda _.3343) '_.3341) . _.3287))))
              ;(conj (== '_.4665 _.3343) (== _.4665 x)))
             ;(pause
              ;(state
               ;((lambda (list '1 (app (lambda _.3343) '_.3341) . _.3287))))
              ;(disj
               ;(conj
                ;(== (list . _.4666) _.3343)
                ;(eval-listo _.4666 (_.3341 (x)) x))
               ;(disj
                ;(conj (== (var _.4667) _.3343) (lookupo _.4667 (_.3341 (x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.4668 _.4669) _.3343)
                    ;(eval-expo _.4668 (_.3341 (x)) (closure _.4672 _.4671)))
                   ;(eval-expo _.4669 (_.3341 (x)) _.4670))
                  ;(eval-expo _.4672 (_.4670 . _.4671) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.4673 _.4674) _.3343)
                     ;(== (_.4675 . _.4676) x))
                    ;(eval-expo _.4673 (_.3341 (x)) _.4675))
                   ;(eval-expo _.4674 (_.3341 (x)) _.4676))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.4677) _.3343) (== _.4678 x))
                    ;(eval-expo _.4677 (_.3341 (x)) (_.4678 . _.4679)))
                   ;(conj
                    ;(conj (== (cdr _.4680) _.3343) (== _.4682 x))
                    ;(eval-expo _.4680 (_.3341 (x)) (_.4681 . _.4682)))))))))))
           ;(disj
            ;(pause
             ;(state
              ;((lambda (list
                        ;'1
                        ;(app (lambda (list . _.4217)) (lambda _.3919))
                        ;.
                        ;_.3287))))
             ;(conj
              ;(conj
               ;(conj (== (_.5427 . _.5428) _.4217) (== (_.5429 . _.5430) x))
               ;(eval-expo _.5427 ((closure _.3919 ((x))) (x)) _.5429))
              ;(eval-listo _.5428 ((closure _.3919 ((x))) (x)) _.5430)))
            ;(disj
             ;(pause
              ;(state
               ;((lambda (list
                         ;'1
                         ;(app (lambda _.3343) (lambda _.3919))
                         ;.
                         ;_.3287))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.4219 _.4220) _.3343)
                  ;(eval-expo
                   ;_.4219
                   ;((closure _.3919 ((x))) (x))
                   ;(closure _.4223 _.4222)))
                 ;(eval-expo _.4220 ((closure _.3919 ((x))) (x)) _.4221))
                ;(eval-expo _.4223 (_.4221 . _.4222) x))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.4224 _.4225) _.3343)
                   ;(== (_.4226 . _.4227) x))
                  ;(eval-expo _.4224 ((closure _.3919 ((x))) (x)) _.4226))
                 ;(eval-expo _.4225 ((closure _.3919 ((x))) (x)) _.4227))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.4228) _.3343) (== _.4229 x))
                  ;(eval-expo
                   ;_.4228
                   ;((closure _.3919 ((x))) (x))
                   ;(_.4229 . _.4230)))
                 ;(conj
                  ;(conj (== (cdr _.4231) _.3343) (== _.4233 x))
                  ;(eval-expo
                   ;_.4231
                   ;((closure _.3919 ((x))) (x))
                   ;(_.4232 . _.4233)))))))
             ;(disj
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app (lambda (var _.4218)) (lambda _.3919))
                          ;.
                          ;_.3287))))
               ;(conj (== () _.4218) (== (closure _.3919 ((x))) x)))
              ;(pause
               ;(state
                ;((lambda (list
                          ;'1
                          ;(app (lambda (var _.4218)) (lambda _.3919))
                          ;.
                          ;_.3287))))
               ;(conj (== (s . _.5977) _.4218) (lookupo _.5977 ((x)) x)))))))
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (list '1 (cdr _.3351) . _.3287))))
              ;(conj
               ;(== (var _.4362) _.3351)
               ;(lookupo _.4362 ((x)) (_.3352 . x))))
             ;(pause
              ;(state ((lambda (list '1 (cdr _.3351) . _.3287))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.4363 _.4364) _.3351)
                  ;(eval-expo _.4363 ((x)) (closure _.4367 _.4366)))
                 ;(eval-expo _.4364 ((x)) _.4365))
                ;(eval-expo _.4367 (_.4365 . _.4366) (_.3352 . x)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.4368 _.4369) _.3351)
                   ;(== (_.4370 . _.4371) (_.3352 . x)))
                  ;(eval-expo _.4368 ((x)) _.4370))
                 ;(eval-expo _.4369 ((x)) _.4371))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.4372) _.3351) (== _.4373 (_.3352 . x)))
                  ;(eval-expo _.4372 ((x)) (_.4373 . _.4374)))
                 ;(conj
                  ;(conj (== (cdr _.4375) _.3351) (== _.4377 (_.3352 . x)))
                  ;(eval-expo _.4375 ((x)) (_.4376 . _.4377))))))))
            ;(pause
             ;(state ((lambda (list '1 (cdr (list . _.4361)) . _.3287))))
             ;(conj
              ;(conj
               ;(conj
                ;(== (_.5540 . _.5541) _.4361)
                ;(== (_.5542 . _.5543) (_.3352 . x)))
               ;(eval-expo _.5540 ((x)) _.5542))
              ;(eval-listo _.5541 ((x)) _.5543))))
           ;(disj
            ;(pause
             ;(state ((lambda (list '1 (car (list . _.4236)) . _.3287))))
             ;(conj
              ;(conj
               ;(conj
                ;(== (_.5469 . _.5470) _.4236)
                ;(== (_.5471 . _.5472) (x . _.3350)))
               ;(eval-expo _.5469 ((x)) _.5471))
              ;(eval-listo _.5470 ((x)) _.5472)))
            ;(disj
             ;(pause
              ;(state ((lambda (list '1 (car _.3348) . _.3287))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.4238 _.4239) _.3348)
                  ;(eval-expo _.4238 ((x)) (closure _.4242 _.4241)))
                 ;(eval-expo _.4239 ((x)) _.4240))
                ;(eval-expo _.4242 (_.4240 . _.4241) (x . _.3350)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.4243 _.4244) _.3348)
                   ;(== (_.4245 . _.4246) (x . _.3350)))
                  ;(eval-expo _.4243 ((x)) _.4245))
                 ;(eval-expo _.4244 ((x)) _.4246))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.4247) _.3348) (== _.4248 (x . _.3350)))
                  ;(eval-expo _.4247 ((x)) (_.4248 . _.4249)))
                 ;(conj
                  ;(conj (== (cdr _.4250) _.3348) (== _.4252 (x . _.3350)))
                  ;(eval-expo _.4250 ((x)) (_.4251 . _.4252)))))))
             ;(disj
              ;(pause
               ;(state ((lambda (list '1 (car (var _.4237)) . _.3287))))
               ;(conj (== () _.4237) (== (x) (x . _.3350))))
              ;(pause
               ;(state ((lambda (list '1 (car (var _.4237)) . _.3287))))
               ;(conj
                ;(== (s . _.6025) _.4237)
                ;(lookupo _.6025 () (x . _.3350)))))))))
         ;(eval-listo _.3287 (_.3091 . _.3092) _.3289)))
       ;(disj
        ;(disj
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cdr _.3158))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.3422 _.3423) _.3158)
                 ;(eval-expo _.3422 ((x)) (closure _.3426 _.3425)))
                ;(eval-expo _.3423 ((x)) _.3424))
               ;(eval-expo _.3426 (_.3424 . _.3425) (_.3159 1 x))))
             ;(pause
              ;(state ((lambda (cdr _.3158))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.3427 _.3428) _.3158)
                  ;(== (_.3429 . _.3430) (_.3159 1 x)))
                 ;(eval-expo _.3427 ((x)) _.3429))
                ;(eval-expo _.3428 ((x)) _.3430))
               ;(disj
                ;(conj
                 ;(conj (== (car _.3431) _.3158) (== _.3432 (_.3159 1 x)))
                 ;(eval-expo _.3431 ((x)) (_.3432 . _.3433)))
                ;(conj
                 ;(conj (== (cdr _.3434) _.3158) (== _.3436 (_.3159 1 x)))
                 ;(eval-expo _.3434 ((x)) (_.3435 . _.3436)))))))
            ;(pause
             ;(state ((lambda (cdr (var _.3421)))))
             ;(conj (== (s . _.4723) _.3421) (lookupo _.4723 () (_.3159 1 x)))))
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cdr (list (lambda _.4993) . _.4379)))))
              ;(conj (== () _.4379) (== () (1 x))))
             ;(pause
              ;(state ((lambda (cdr (list (lambda _.4993) . _.4379)))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (_.5492 . _.5493) _.4379)
                 ;(== (_.5494 . _.5495) (1 x)))
                ;(eval-expo _.5492 ((x)) _.5494))
               ;(eval-listo _.5493 ((x)) _.5495))))
            ;(conj
             ;(disj
              ;(pause
               ;(state ((lambda (cdr (list _.4378 . _.4379)))))
               ;(conj (== '_.4994 _.4378) (== _.4994 _.3159)))
              ;(pause
               ;(state ((lambda (cdr (list _.4378 . _.4379)))))
               ;(disj
                ;(conj
                 ;(== (list . _.4995) _.4378)
                 ;(eval-listo _.4995 ((x)) _.3159))
                ;(disj
                 ;(conj (== (var _.4996) _.4378) (lookupo _.4996 ((x)) _.3159))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (app _.4997 _.4998) _.4378)
                     ;(eval-expo _.4997 ((x)) (closure _.5001 _.5000)))
                    ;(eval-expo _.4998 ((x)) _.4999))
                   ;(eval-expo _.5001 (_.4999 . _.5000) _.3159))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (cons _.5002 _.5003) _.4378)
                      ;(== (_.5004 . _.5005) _.3159))
                     ;(eval-expo _.5002 ((x)) _.5004))
                    ;(eval-expo _.5003 ((x)) _.5005))
                   ;(disj
                    ;(conj
                     ;(conj (== (car _.5006) _.4378) (== _.5007 _.3159))
                     ;(eval-expo _.5006 ((x)) (_.5007 . _.5008)))
                    ;(conj
                     ;(conj (== (cdr _.5009) _.4378) (== _.5011 _.3159))
                     ;(eval-expo _.5009 ((x)) (_.5010 . _.5011))))))))))
             ;(eval-listo _.4379 (_.3091 . _.3092) _.4381))))
          ;(disj
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (car (list _.4295 . _.4296)))))
              ;(conj (== '_.4808 _.4295) (== _.4808 (1 x))))
             ;(pause
              ;(state ((lambda (car (list _.4295 . _.4296)))))
              ;(disj
               ;(conj
                ;(== (list . _.4809) _.4295)
                ;(eval-listo _.4809 ((x)) (1 x)))
               ;(disj
                ;(conj (== (var _.4810) _.4295) (lookupo _.4810 ((x)) (1 x)))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.4811 _.4812) _.4295)
                    ;(eval-expo _.4811 ((x)) (closure _.4815 _.4814)))
                   ;(eval-expo _.4812 ((x)) _.4813))
                  ;(eval-expo _.4815 (_.4813 . _.4814) (1 x)))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.4816 _.4817) _.4295)
                     ;(== (_.4818 . _.4819) (1 x)))
                    ;(eval-expo _.4816 ((x)) _.4818))
                   ;(eval-expo _.4817 ((x)) _.4819))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.4820) _.4295) (== _.4821 (1 x)))
                    ;(eval-expo _.4820 ((x)) (_.4821 . _.4822)))
                   ;(conj
                    ;(conj (== (cdr _.4823) _.4295) (== _.4825 (1 x)))
                    ;(eval-expo _.4823 ((x)) (_.4824 . _.4825))))))))))
            ;(eval-listo _.4296 (_.3091 . _.3092) _.4298))
           ;(disj
            ;(pause
             ;(state ((lambda (car (var _.3398)))))
             ;(conj
              ;(== (s . _.4598) _.3398)
              ;(lookupo _.4598 () ((1 x) . _.3157))))
            ;(disj
             ;(pause
              ;(state ((lambda (car _.3155))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.3404 _.3405) _.3155)
                  ;(== (_.3406 . _.3407) ((1 x) . _.3157)))
                 ;(eval-expo _.3404 ((x)) _.3406))
                ;(eval-expo _.3405 ((x)) _.3407))
               ;(disj
                ;(conj
                 ;(conj (== (car _.3408) _.3155) (== _.3409 ((1 x) . _.3157)))
                 ;(eval-expo _.3408 ((x)) (_.3409 . _.3410)))
                ;(conj
                 ;(conj (== (cdr _.3411) _.3155) (== _.3413 ((1 x) . _.3157)))
                 ;(eval-expo _.3411 ((x)) (_.3412 . _.3413))))))
             ;(conj
              ;(conj
               ;(disj
                ;(pause
                 ;(state ((lambda (car (app _.3399 _.3400)))))
                 ;(conj
                  ;(== (lambda _.5937) _.3399)
                  ;(== (closure _.5937 ((x))) (closure _.3403 _.3402))))
                ;(pause
                 ;(state ((lambda (car (app _.3399 _.3400)))))
                 ;(disj
                  ;(conj
                   ;(== '_.5938 _.3399)
                   ;(== _.5938 (closure _.3403 _.3402)))
                  ;(disj
                   ;(conj
                    ;(== (list . _.5939) _.3399)
                    ;(eval-listo _.5939 ((x)) (closure _.3403 _.3402)))
                   ;(disj
                    ;(conj
                     ;(== (var _.5940) _.3399)
                     ;(lookupo _.5940 ((x)) (closure _.3403 _.3402)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (app _.5941 _.5942) _.3399)
                        ;(eval-expo _.5941 ((x)) (closure _.5945 _.5944)))
                       ;(eval-expo _.5942 ((x)) _.5943))
                      ;(eval-expo
                       ;_.5945
                       ;(_.5943 . _.5944)
                       ;(closure _.3403 _.3402)))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(conj
                         ;(== (cons _.5946 _.5947) _.3399)
                         ;(== (_.5948 . _.5949) (closure _.3403 _.3402)))
                        ;(eval-expo _.5946 ((x)) _.5948))
                       ;(eval-expo _.5947 ((x)) _.5949))
                      ;(disj
                       ;(conj
                        ;(conj
                         ;(== (car _.5950) _.3399)
                         ;(== _.5951 (closure _.3403 _.3402)))
                        ;(eval-expo _.5950 ((x)) (_.5951 . _.5952)))
                       ;(conj
                        ;(conj
                         ;(== (cdr _.5953) _.3399)
                         ;(== _.5955 (closure _.3403 _.3402)))
                        ;(eval-expo _.5953 ((x)) (_.5954 . _.5955)))))))))))
               ;(eval-expo _.3400 (_.3091 . _.3092) _.3401))
              ;(eval-expo _.3403 (_.3401 . _.3402) (_.3156 . _.3157)))))))
         ;(disj
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (cons '1 _.3152))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.3464 _.3465) _.3152)
                 ;(eval-expo _.3464 ((x)) (closure _.3468 _.3467)))
                ;(eval-expo _.3465 ((x)) _.3466))
               ;(eval-expo _.3468 (_.3466 . _.3467) (x))))
             ;(pause
              ;(state ((lambda (cons '1 _.3152))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.3469 _.3470) _.3152)
                  ;(== (_.3471 . _.3472) (x)))
                 ;(eval-expo _.3469 ((x)) _.3471))
                ;(eval-expo _.3470 ((x)) _.3472))
               ;(disj
                ;(conj
                 ;(conj (== (car _.3473) _.3152) (== _.3474 (x)))
                 ;(eval-expo _.3473 ((x)) (_.3474 . _.3475)))
                ;(conj
                 ;(conj (== (cdr _.3476) _.3152) (== _.3478 (x)))
                 ;(eval-expo _.3476 ((x)) (_.3477 . _.3478)))))))
            ;(pause
             ;(state ((lambda (cons '1 (var _.3463)))))
             ;(conj (== (s . _.4745) _.3463) (lookupo _.4745 () (x)))))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (cons '1 (list _.4405 . _.4406)))))
              ;(conj (== '_.5040 _.4405) (== _.5040 x)))
             ;(pause
              ;(state ((lambda (cons '1 (list _.4405 . _.4406)))))
              ;(disj
               ;(conj (== (list . _.5041) _.4405) (eval-listo _.5041 ((x)) x))
               ;(disj
                ;(conj (== (var _.5042) _.4405) (lookupo _.5042 ((x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.5043 _.5044) _.4405)
                    ;(eval-expo _.5043 ((x)) (closure _.5047 _.5046)))
                   ;(eval-expo _.5044 ((x)) _.5045))
                  ;(eval-expo _.5047 (_.5045 . _.5046) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.5048 _.5049) _.4405)
                     ;(== (_.5050 . _.5051) x))
                    ;(eval-expo _.5048 ((x)) _.5050))
                   ;(eval-expo _.5049 ((x)) _.5051))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.5052) _.4405) (== _.5053 x))
                    ;(eval-expo _.5052 ((x)) (_.5053 . _.5054)))
                   ;(conj
                    ;(conj (== (cdr _.5055) _.4405) (== _.5057 x))
                    ;(eval-expo _.5055 ((x)) (_.5056 . _.5057))))))))))
            ;(eval-listo _.4406 (_.3091 . _.3092) _.4408)))
          ;(conj
           ;(disj
            ;(pause
             ;(state ((lambda (cons _.3151 _.3152))))
             ;(disj
              ;(conj
               ;(conj (== (car _.3329) _.3151) (== _.3330 1))
               ;(eval-expo _.3329 ((x)) (_.3330 . _.3331)))
              ;(conj
               ;(conj (== (cdr _.3332) _.3151) (== _.3334 1))
               ;(eval-expo _.3332 ((x)) (_.3333 . _.3334)))))
            ;(conj
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (cons (app (lambda _.3324) _.3321) _.3152))))
                ;(conj
                 ;(== (lambda _.5575) _.3321)
                 ;(== (closure _.5575 ((x))) _.3322)))
               ;(pause
                ;(state ((lambda (cons (app (lambda _.3324) _.3321) _.3152))))
                ;(disj
                 ;(conj (== '_.5576 _.3321) (== _.5576 _.3322))
                 ;(disj
                  ;(conj
                   ;(== (list . _.5577) _.3321)
                   ;(eval-listo _.5577 ((x)) _.3322))
                  ;(disj
                   ;(conj
                    ;(== (var _.5578) _.3321)
                    ;(lookupo _.5578 ((x)) _.3322))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (app _.5579 _.5580) _.3321)
                       ;(eval-expo _.5579 ((x)) (closure _.5583 _.5582)))
                      ;(eval-expo _.5580 ((x)) _.5581))
                     ;(eval-expo _.5583 (_.5581 . _.5582) _.3322))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (cons _.5584 _.5585) _.3321)
                        ;(== (_.5586 . _.5587) _.3322))
                       ;(eval-expo _.5584 ((x)) _.5586))
                      ;(eval-expo _.5585 ((x)) _.5587))
                     ;(disj
                      ;(conj
                       ;(conj (== (car _.5588) _.3321) (== _.5589 _.3322))
                       ;(eval-expo _.5588 ((x)) (_.5589 . _.5590)))
                      ;(conj
                       ;(conj (== (cdr _.5591) _.3321) (== _.5593 _.3322))
                       ;(eval-expo _.5591 ((x)) (_.5592 . _.5593)))))))))))
              ;(conj
               ;(disj
                ;(pause
                 ;(state ((lambda (cons (app _.3320 _.3321) _.3152))))
                 ;(conj
                  ;(== '_.4873 _.3320)
                  ;(== _.4873 (closure _.3324 _.3323))))
                ;(pause
                 ;(state ((lambda (cons (app _.3320 _.3321) _.3152))))
                 ;(disj
                  ;(conj
                   ;(== (list . _.4874) _.3320)
                   ;(eval-listo _.4874 ((x)) (closure _.3324 _.3323)))
                  ;(disj
                   ;(conj
                    ;(== (var _.4875) _.3320)
                    ;(lookupo _.4875 ((x)) (closure _.3324 _.3323)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (app _.4876 _.4877) _.3320)
                       ;(eval-expo _.4876 ((x)) (closure _.4880 _.4879)))
                      ;(eval-expo _.4877 ((x)) _.4878))
                     ;(eval-expo
                      ;_.4880
                      ;(_.4878 . _.4879)
                      ;(closure _.3324 _.3323)))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(conj
                        ;(== (cons _.4881 _.4882) _.3320)
                        ;(== (_.4883 . _.4884) (closure _.3324 _.3323)))
                       ;(eval-expo _.4881 ((x)) _.4883))
                      ;(eval-expo _.4882 ((x)) _.4884))
                     ;(disj
                      ;(conj
                       ;(conj
                        ;(== (car _.4885) _.3320)
                        ;(== _.4886 (closure _.3324 _.3323)))
                       ;(eval-expo _.4885 ((x)) (_.4886 . _.4887)))
                      ;(conj
                       ;(conj
                        ;(== (cdr _.4888) _.3320)
                        ;(== _.4890 (closure _.3324 _.3323)))
                       ;(eval-expo _.4888 ((x)) (_.4889 . _.4890))))))))))
               ;(eval-expo _.3321 (_.3091 . _.3092) _.3322)))
             ;(eval-expo _.3324 (_.3322 . _.3323) _.3153)))
           ;(eval-expo _.3152 (_.3091 . _.3092) _.3154))))
        ;(disj
         ;(disj
          ;(disj
           ;(disj
            ;(pause
             ;(state ((lambda (app (lambda _.3150) (lambda _.3293)))))
             ;(disj
              ;(conj
               ;(conj (== (car _.3367) _.3150) (== _.3368 (1 x)))
               ;(eval-expo
                ;_.3367
                ;((closure _.3293 ((x))) (x))
                ;(_.3368 . _.3369)))
              ;(conj
               ;(conj (== (cdr _.3370) _.3150) (== _.3372 (1 x)))
               ;(eval-expo
                ;_.3370
                ;((closure _.3293 ((x))) (x))
                ;(_.3371 . _.3372)))))
            ;(conj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app (lambda (cons _.3363 _.3364)) (lambda _.3293)))))
               ;(conj
                ;(== (lambda _.5651) _.3363)
                ;(== (closure _.5651 ((closure _.3293 ((x))) (x))) 1)))
              ;(pause
               ;(state
                ;((lambda (app (lambda (cons _.3363 _.3364)) (lambda _.3293)))))
               ;(disj
                ;(conj (== '_.5652 _.3363) (== _.5652 1))
                ;(disj
                 ;(conj
                  ;(== (list . _.5653) _.3363)
                  ;(eval-listo _.5653 ((closure _.3293 ((x))) (x)) 1))
                 ;(disj
                  ;(conj
                   ;(== (var _.5654) _.3363)
                   ;(lookupo _.5654 ((closure _.3293 ((x))) (x)) 1))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.5655 _.5656) _.3363)
                      ;(eval-expo
                       ;_.5655
                       ;((closure _.3293 ((x))) (x))
                       ;(closure _.5659 _.5658)))
                     ;(eval-expo _.5656 ((closure _.3293 ((x))) (x)) _.5657))
                    ;(eval-expo _.5659 (_.5657 . _.5658) 1))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.5660 _.5661) _.3363)
                       ;(== (_.5662 . _.5663) 1))
                      ;(eval-expo _.5660 ((closure _.3293 ((x))) (x)) _.5662))
                     ;(eval-expo _.5661 ((closure _.3293 ((x))) (x)) _.5663))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.5664) _.3363) (== _.5665 1))
                      ;(eval-expo
                       ;_.5664
                       ;((closure _.3293 ((x))) (x))
                       ;(_.5665 . _.5666)))
                     ;(conj
                      ;(conj (== (cdr _.5667) _.3363) (== _.5669 1))
                      ;(eval-expo
                       ;_.5667
                       ;((closure _.3293 ((x))) (x))
                       ;(_.5668 . _.5669)))))))))))
             ;(eval-expo _.3364 (_.3148 . _.3149) _.3366)))
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (app (lambda _.3362) _.3359))
                          ;(lambda _.3293)))))
               ;(conj
                ;(== (lambda _.5347) _.3359)
                ;(== (closure _.5347 ((closure _.3293 ((x))) (x))) _.3360)))
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (app (lambda _.3362) _.3359))
                          ;(lambda _.3293)))))
               ;(disj
                ;(conj (== '_.5348 _.3359) (== _.5348 _.3360))
                ;(disj
                 ;(conj
                  ;(== (list . _.5349) _.3359)
                  ;(eval-listo _.5349 ((closure _.3293 ((x))) (x)) _.3360))
                 ;(disj
                  ;(conj
                   ;(== (var _.5350) _.3359)
                   ;(lookupo _.5350 ((closure _.3293 ((x))) (x)) _.3360))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.5351 _.5352) _.3359)
                      ;(eval-expo
                       ;_.5351
                       ;((closure _.3293 ((x))) (x))
                       ;(closure _.5355 _.5354)))
                     ;(eval-expo _.5352 ((closure _.3293 ((x))) (x)) _.5353))
                    ;(eval-expo _.5355 (_.5353 . _.5354) _.3360))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.5356 _.5357) _.3359)
                       ;(== (_.5358 . _.5359) _.3360))
                      ;(eval-expo _.5356 ((closure _.3293 ((x))) (x)) _.5358))
                     ;(eval-expo _.5357 ((closure _.3293 ((x))) (x)) _.5359))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.5360) _.3359) (== _.5361 _.3360))
                      ;(eval-expo
                       ;_.5360
                       ;((closure _.3293 ((x))) (x))
                       ;(_.5361 . _.5362)))
                     ;(conj
                      ;(conj (== (cdr _.5363) _.3359) (== _.5365 _.3360))
                      ;(eval-expo
                       ;_.5363
                       ;((closure _.3293 ((x))) (x))
                       ;(_.5364 . _.5365)))))))))))
             ;(conj
              ;(disj
               ;(pause
                ;(state
                 ;((lambda (app (lambda (app _.3358 _.3359)) (lambda _.3293)))))
                ;(conj (== '_.4475 _.3358) (== _.4475 (closure _.3362 _.3361))))
               ;(pause
                ;(state
                 ;((lambda (app (lambda (app _.3358 _.3359)) (lambda _.3293)))))
                ;(disj
                 ;(conj
                  ;(== (list . _.4476) _.3358)
                  ;(eval-listo
                   ;_.4476
                   ;((closure _.3293 ((x))) (x))
                   ;(closure _.3362 _.3361)))
                 ;(disj
                  ;(conj
                   ;(== (var _.4477) _.3358)
                   ;(lookupo
                    ;_.4477
                    ;((closure _.3293 ((x))) (x))
                    ;(closure _.3362 _.3361)))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.4478 _.4479) _.3358)
                      ;(eval-expo
                       ;_.4478
                       ;((closure _.3293 ((x))) (x))
                       ;(closure _.4482 _.4481)))
                     ;(eval-expo _.4479 ((closure _.3293 ((x))) (x)) _.4480))
                    ;(eval-expo
                     ;_.4482
                     ;(_.4480 . _.4481)
                     ;(closure _.3362 _.3361)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.4483 _.4484) _.3358)
                       ;(== (_.4485 . _.4486) (closure _.3362 _.3361)))
                      ;(eval-expo _.4483 ((closure _.3293 ((x))) (x)) _.4485))
                     ;(eval-expo _.4484 ((closure _.3293 ((x))) (x)) _.4486))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.4487) _.3358)
                       ;(== _.4488 (closure _.3362 _.3361)))
                      ;(eval-expo
                       ;_.4487
                       ;((closure _.3293 ((x))) (x))
                       ;(_.4488 . _.4489)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.4490) _.3358)
                       ;(== _.4492 (closure _.3362 _.3361)))
                      ;(eval-expo
                       ;_.4490
                       ;((closure _.3293 ((x))) (x))
                       ;(_.4491 . _.4492))))))))))
              ;(eval-expo _.3359 (_.3148 . _.3149) _.3360)))
            ;(eval-expo _.3362 (_.3360 . _.3361) _.3083)))
          ;(disj
           ;(conj
            ;(pause
             ;(state
              ;((lambda (app
                        ;(lambda (list '1 _.4535 . _.4536))
                        ;(lambda _.3293)))))
             ;(disj
              ;(conj (== '_.5432 _.4535) (== _.5432 x))
              ;(disj
               ;(conj
                ;(== (list . _.5433) _.4535)
                ;(eval-listo _.5433 ((closure _.3293 ((x))) (x)) x))
               ;(disj
                ;(conj
                 ;(== (var _.5434) _.4535)
                 ;(lookupo _.5434 ((closure _.3293 ((x))) (x)) x))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.5435 _.5436) _.4535)
                    ;(eval-expo
                     ;_.5435
                     ;((closure _.3293 ((x))) (x))
                     ;(closure _.5439 _.5438)))
                   ;(eval-expo _.5436 ((closure _.3293 ((x))) (x)) _.5437))
                  ;(eval-expo _.5439 (_.5437 . _.5438) x))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.5440 _.5441) _.4535)
                     ;(== (_.5442 . _.5443) x))
                    ;(eval-expo _.5440 ((closure _.3293 ((x))) (x)) _.5442))
                   ;(eval-expo _.5441 ((closure _.3293 ((x))) (x)) _.5443))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.5444) _.4535) (== _.5445 x))
                    ;(eval-expo
                     ;_.5444
                     ;((closure _.3293 ((x))) (x))
                     ;(_.5445 . _.5446)))
                   ;(conj
                    ;(conj (== (cdr _.5447) _.4535) (== _.5449 x))
                    ;(eval-expo
                     ;_.5447
                     ;((closure _.3293 ((x))) (x))
                     ;(_.5448 . _.5449))))))))))
            ;(eval-listo _.4536 (_.3148 . _.3149) _.4538))
           ;(conj
            ;(disj
             ;(disj
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (list _.3687 . _.3688))
                          ;(lambda _.3293)))))
               ;(conj
                ;(== (var _.4100) _.3687)
                ;(lookupo _.4100 ((closure _.3293 ((x))) (x)) 1)))
              ;(pause
               ;(state
                ;((lambda (app
                          ;(lambda (list _.3687 . _.3688))
                          ;(lambda _.3293)))))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (app _.4101 _.4102) _.3687)
                   ;(eval-expo
                    ;_.4101
                    ;((closure _.3293 ((x))) (x))
                    ;(closure _.4105 _.4104)))
                  ;(eval-expo _.4102 ((closure _.3293 ((x))) (x)) _.4103))
                 ;(eval-expo _.4105 (_.4103 . _.4104) 1))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (cons _.4106 _.4107) _.3687)
                    ;(== (_.4108 . _.4109) 1))
                   ;(eval-expo _.4106 ((closure _.3293 ((x))) (x)) _.4108))
                  ;(eval-expo _.4107 ((closure _.3293 ((x))) (x)) _.4109))
                 ;(disj
                  ;(conj
                   ;(conj (== (car _.4110) _.3687) (== _.4111 1))
                   ;(eval-expo
                    ;_.4110
                    ;((closure _.3293 ((x))) (x))
                    ;(_.4111 . _.4112)))
                  ;(conj
                   ;(conj (== (cdr _.4113) _.3687) (== _.4115 1))
                   ;(eval-expo
                    ;_.4113
                    ;((closure _.3293 ((x))) (x))
                    ;(_.4114 . _.4115))))))))
             ;(pause
              ;(state
               ;((lambda (app
                         ;(lambda (list (list . _.4099) . _.3688))
                         ;(lambda _.3293)))))
              ;(conj
               ;(conj
                ;(conj (== (_.5237 . _.5238) _.4099) (== (_.5239 . _.5240) 1))
                ;(eval-expo _.5237 ((closure _.3293 ((x))) (x)) _.5239))
               ;(eval-listo _.5238 ((closure _.3293 ((x))) (x)) _.5240))))
            ;(eval-listo _.3688 (_.3148 . _.3149) _.3690))))
         ;(disj
          ;(disj
           ;(conj
            ;(disj
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (app (lambda _.3150) (list . _.3295)))))
                ;(conj (== () _.3295) (== () _.3148)))
               ;(pause
                ;(state ((lambda (app (lambda _.3150) (list . _.3295)))))
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (_.4512 . _.4513) _.3295)
                   ;(== (_.4514 . _.4515) _.3148))
                  ;(eval-expo _.4512 ((x)) _.4514))
                 ;(eval-listo _.4513 ((x)) _.4515))))
              ;(disj
               ;(pause
                ;(state ((lambda (app (lambda _.3150) _.3147))))
                ;(conj (== (var _.3296) _.3147) (lookupo _.3296 ((x)) _.3148)))
               ;(pause
                ;(state ((lambda (app (lambda _.3150) _.3147))))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.3297 _.3298) _.3147)
                    ;(eval-expo _.3297 ((x)) (closure _.3301 _.3300)))
                   ;(eval-expo _.3298 ((x)) _.3299))
                  ;(eval-expo _.3301 (_.3299 . _.3300) _.3148))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.3302 _.3303) _.3147)
                     ;(== (_.3304 . _.3305) _.3148))
                    ;(eval-expo _.3302 ((x)) _.3304))
                   ;(eval-expo _.3303 ((x)) _.3305))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.3306) _.3147) (== _.3307 _.3148))
                    ;(eval-expo _.3306 ((x)) (_.3307 . _.3308)))
                   ;(conj
                    ;(conj (== (cdr _.3309) _.3147) (== _.3311 _.3148))
                    ;(eval-expo _.3309 ((x)) (_.3310 . _.3311)))))))))
             ;(disj
              ;(disj
               ;(pause
                ;(state ((lambda (app '(closure _.3150 _.3149) _.3147))))
                ;(conj (== '_.3374 _.3147) (== _.3374 _.3148)))
               ;(pause
                ;(state ((lambda (app '(closure _.3150 _.3149) _.3147))))
                ;(disj
                 ;(conj
                  ;(== (list . _.3375) _.3147)
                  ;(eval-listo _.3375 ((x)) _.3148))
                 ;(disj
                  ;(conj (== (var _.3376) _.3147) (lookupo _.3376 ((x)) _.3148))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.3377 _.3378) _.3147)
                      ;(eval-expo _.3377 ((x)) (closure _.3381 _.3380)))
                     ;(eval-expo _.3378 ((x)) _.3379))
                    ;(eval-expo _.3381 (_.3379 . _.3380) _.3148))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.3382 _.3383) _.3147)
                       ;(== (_.3384 . _.3385) _.3148))
                      ;(eval-expo _.3382 ((x)) _.3384))
                     ;(eval-expo _.3383 ((x)) _.3385))
                    ;(disj
                     ;(conj
                      ;(conj (== (car _.3386) _.3147) (== _.3387 _.3148))
                      ;(eval-expo _.3386 ((x)) (_.3387 . _.3388)))
                     ;(conj
                      ;(conj (== (cdr _.3389) _.3147) (== _.3391 _.3148))
                      ;(eval-expo _.3389 ((x)) (_.3390 . _.3391))))))))))
              ;(conj
               ;(disj
                ;(disj
                 ;(pause
                  ;(state ((lambda (app (list . _.3250) _.3147))))
                  ;(conj (== () _.3250) (== () (closure _.3150 _.3149))))
                 ;(pause
                  ;(state ((lambda (app (list . _.3250) _.3147))))
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (_.4291 . _.4292) _.3250)
                     ;(== (_.4293 . _.4294) (closure _.3150 _.3149)))
                    ;(eval-expo _.4291 ((x)) _.4293))
                   ;(eval-listo _.4292 ((x)) _.4294))))
                ;(disj
                 ;(pause
                  ;(state ((lambda (app _.3146 _.3147))))
                  ;(conj
                   ;(== (var _.3251) _.3146)
                   ;(lookupo _.3251 ((x)) (closure _.3150 _.3149))))
                 ;(pause
                  ;(state ((lambda (app _.3146 _.3147))))
                  ;(disj
                   ;(conj
                    ;(conj
                     ;(conj
                      ;(== (app _.3252 _.3253) _.3146)
                      ;(eval-expo _.3252 ((x)) (closure _.3256 _.3255)))
                     ;(eval-expo _.3253 ((x)) _.3254))
                    ;(eval-expo
                     ;_.3256
                     ;(_.3254 . _.3255)
                     ;(closure _.3150 _.3149)))
                   ;(disj
                    ;(conj
                     ;(conj
                      ;(conj
                       ;(== (cons _.3257 _.3258) _.3146)
                       ;(== (_.3259 . _.3260) (closure _.3150 _.3149)))
                      ;(eval-expo _.3257 ((x)) _.3259))
                     ;(eval-expo _.3258 ((x)) _.3260))
                    ;(disj
                     ;(conj
                      ;(conj
                       ;(== (car _.3261) _.3146)
                       ;(== _.3262 (closure _.3150 _.3149)))
                      ;(eval-expo _.3261 ((x)) (_.3262 . _.3263)))
                     ;(conj
                      ;(conj
                       ;(== (cdr _.3264) _.3146)
                       ;(== _.3266 (closure _.3150 _.3149)))
                      ;(eval-expo _.3264 ((x)) (_.3265 . _.3266)))))))))
               ;(eval-expo _.3147 (_.3091 . _.3092) _.3148))))
            ;(eval-expo _.3150 (_.3148 . _.3149) _.3083))
           ;(disj
            ;(disj
             ;(pause
              ;(state
               ;((lambda (app
                         ;'(closure (list . _.3647) _.3149)
                         ;(lambda _.3373)))))
              ;(conj (== () _.3647) (== () (1 x))))
             ;(pause
              ;(state
               ;((lambda (app
                         ;'(closure (list . _.3647) _.3149)
                         ;(lambda _.3373)))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (_.5544 . _.5545) _.3647)
                 ;(== (_.5546 . _.5547) (1 x)))
                ;(eval-expo _.5544 ((closure _.3373 ((x))) . _.3149) _.5546))
               ;(eval-listo _.5545 ((closure _.3373 ((x))) . _.3149) _.5547))))
            ;(disj
             ;(pause
              ;(state ((lambda (app '(closure _.3150 _.3149) (lambda _.3373)))))
              ;(conj
               ;(== (var _.3648) _.3150)
               ;(lookupo _.3648 ((closure _.3373 ((x))) . _.3149) (1 x))))
             ;(pause
              ;(state ((lambda (app '(closure _.3150 _.3149) (lambda _.3373)))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.3649 _.3650) _.3150)
                  ;(eval-expo
                   ;_.3649
                   ;((closure _.3373 ((x))) . _.3149)
                   ;(closure _.3653 _.3652)))
                 ;(eval-expo _.3650 ((closure _.3373 ((x))) . _.3149) _.3651))
                ;(eval-expo _.3653 (_.3651 . _.3652) (1 x)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.3654 _.3655) _.3150)
                   ;(== (_.3656 . _.3657) (1 x)))
                  ;(eval-expo _.3654 ((closure _.3373 ((x))) . _.3149) _.3656))
                 ;(eval-expo _.3655 ((closure _.3373 ((x))) . _.3149) _.3657))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.3658) _.3150) (== _.3659 (1 x)))
                  ;(eval-expo
                   ;_.3658
                   ;((closure _.3373 ((x))) . _.3149)
                   ;(_.3659 . _.3660)))
                 ;(conj
                  ;(conj (== (cdr _.3661) _.3150) (== _.3663 (1 x)))
                  ;(eval-expo
                   ;_.3661
                   ;((closure _.3373 ((x))) . _.3149)
                   ;(_.3662 . _.3663))))))))))
          ;(disj
           ;(disj
            ;(disj
             ;(pause
              ;(state ((lambda (app (lambda _.3150) '_.3148))))
              ;(conj
               ;(conj
                ;(conj
                 ;(== (app _.3570 _.3571) _.3150)
                 ;(eval-expo _.3570 (_.3148 (x)) (closure _.3574 _.3573)))
                ;(eval-expo _.3571 (_.3148 (x)) _.3572))
               ;(eval-expo _.3574 (_.3572 . _.3573) (1 x))))
             ;(pause
              ;(state ((lambda (app (lambda _.3150) '_.3148))))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (cons _.3575 _.3576) _.3150)
                  ;(== (_.3577 . _.3578) (1 x)))
                 ;(eval-expo _.3575 (_.3148 (x)) _.3577))
                ;(eval-expo _.3576 (_.3148 (x)) _.3578))
               ;(disj
                ;(conj
                 ;(conj (== (car _.3579) _.3150) (== _.3580 (1 x)))
                 ;(eval-expo _.3579 (_.3148 (x)) (_.3580 . _.3581)))
                ;(conj
                 ;(conj (== (cdr _.3582) _.3150) (== _.3584 (1 x)))
                 ;(eval-expo _.3582 (_.3148 (x)) (_.3583 . _.3584)))))))
            ;(pause
             ;(state ((lambda (app (lambda (var _.3569)) '_.3148))))
             ;(conj (== (s . _.4950) _.3569) (lookupo _.4950 ((x)) (1 x)))))
           ;(conj
            ;(disj
             ;(pause
              ;(state ((lambda (app (lambda (list _.4451 . _.4452)) '_.3148))))
              ;(conj (== '_.5268 _.4451) (== _.5268 1)))
             ;(pause
              ;(state ((lambda (app (lambda (list _.4451 . _.4452)) '_.3148))))
              ;(disj
               ;(conj
                ;(== (list . _.5269) _.4451)
                ;(eval-listo _.5269 (_.3148 (x)) 1))
               ;(disj
                ;(conj (== (var _.5270) _.4451) (lookupo _.5270 (_.3148 (x)) 1))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(conj
                    ;(== (app _.5271 _.5272) _.4451)
                    ;(eval-expo _.5271 (_.3148 (x)) (closure _.5275 _.5274)))
                   ;(eval-expo _.5272 (_.3148 (x)) _.5273))
                  ;(eval-expo _.5275 (_.5273 . _.5274) 1))
                 ;(disj
                  ;(conj
                   ;(conj
                    ;(conj
                     ;(== (cons _.5276 _.5277) _.4451)
                     ;(== (_.5278 . _.5279) 1))
                    ;(eval-expo _.5276 (_.3148 (x)) _.5278))
                   ;(eval-expo _.5277 (_.3148 (x)) _.5279))
                  ;(disj
                   ;(conj
                    ;(conj (== (car _.5280) _.4451) (== _.5281 1))
                    ;(eval-expo _.5280 (_.3148 (x)) (_.5281 . _.5282)))
                   ;(conj
                    ;(conj (== (cdr _.5283) _.4451) (== _.5285 1))
                    ;(eval-expo _.5283 (_.3148 (x)) (_.5284 . _.5285))))))))))
            ;(eval-listo _.4452 (_.3148 . _.3149) _.4454)))))))
      ;(eval-listo _.3082 () _.3084))
     ;(conj
      ;(disj
       ;(conj
        ;(disj
         ;(disj
          ;(pause
           ;(state ((lambda (app (lambda (var ())) '(1 x)))))
           ;(conj (== (lambda _.6000) '(y)) (== (closure _.6000 ()) _.5825)))
          ;(pause
           ;(state ((lambda (app (lambda (var ())) '(1 x)))))
           ;(disj
            ;(conj (== '_.6001 '(y)) (== _.6001 _.5825))
            ;(disj
             ;(conj (== (list . _.6002) '(y)) (eval-listo _.6002 () _.5825))
             ;(disj
              ;(conj (== (var _.6003) '(y)) (lookupo _.6003 () _.5825))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(== (app _.6004 _.6005) '(y))
                  ;(eval-expo _.6004 () (closure _.6008 _.6007)))
                 ;(eval-expo _.6005 () _.6006))
                ;(eval-expo _.6008 (_.6006 . _.6007) _.5825))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(== (cons _.6009 _.6010) '(y))
                   ;(== (_.6011 . _.6012) _.5825))
                  ;(eval-expo _.6009 () _.6011))
                 ;(eval-expo _.6010 () _.6012))
                ;(disj
                 ;(conj
                  ;(conj (== (car _.6013) '(y)) (== _.6014 _.5825))
                  ;(eval-expo _.6013 () (_.6014 . _.6015)))
                 ;(conj
                  ;(conj (== (cdr _.6016) '(y)) (== _.6018 _.5825))
                  ;(eval-expo _.6016 () (_.6017 . _.6018)))))))))))
         ;(conj
          ;(disj
           ;(pause
            ;(state ((lambda (app (lambda (var ())) '(1 x)))))
            ;(conj
             ;(== '_.5979 (lambda (app (lambda (var ())) '(1 x))))
             ;(== _.5979 (closure _.5827 _.5826))))
           ;(pause
            ;(state ((lambda (app (lambda (var ())) '(1 x)))))
            ;(disj
             ;(conj
              ;(== (list . _.5980) (lambda (app (lambda (var ())) '(1 x))))
              ;(eval-listo _.5980 () (closure _.5827 _.5826)))
             ;(disj
              ;(conj
               ;(== (var _.5981) (lambda (app (lambda (var ())) '(1 x))))
               ;(lookupo _.5981 () (closure _.5827 _.5826)))
              ;(disj
               ;(conj
                ;(conj
                 ;(conj
                  ;(==
                   ;(app _.5982 _.5983)
                   ;(lambda (app (lambda (var ())) '(1 x))))
                  ;(eval-expo _.5982 () (closure _.5986 _.5985)))
                 ;(eval-expo _.5983 () _.5984))
                ;(eval-expo _.5986 (_.5984 . _.5985) (closure _.5827 _.5826)))
               ;(disj
                ;(conj
                 ;(conj
                  ;(conj
                   ;(==
                    ;(cons _.5987 _.5988)
                    ;(lambda (app (lambda (var ())) '(1 x))))
                   ;(== (_.5989 . _.5990) (closure _.5827 _.5826)))
                  ;(eval-expo _.5987 () _.5989))
                 ;(eval-expo _.5988 () _.5990))
                ;(disj
                 ;(conj
                  ;(conj
                   ;(== (car _.5991) (lambda (app (lambda (var ())) '(1 x))))
                   ;(== _.5992 (closure _.5827 _.5826)))
                  ;(eval-expo _.5991 () (_.5992 . _.5993)))
                 ;(conj
                  ;(conj
                   ;(== (cdr _.5994) (lambda (app (lambda (var ())) '(1 x))))
                   ;(== _.5996 (closure _.5827 _.5826)))
                  ;(eval-expo _.5994 () (_.5995 . _.5996))))))))))
          ;(eval-expo _.5824 () _.5825)))
        ;(eval-expo _.5827 (_.5825 . _.5826) _.5779))
       ;(disj
        ;(pause
         ;(state ((lambda (app (lambda (var ())) '(1 x)))))
         ;(conj
          ;(conj
           ;(== (car _.5832) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           ;(== _.5833 (1 y)))
          ;(eval-expo _.5832 () (_.5833 . _.5834))))
        ;(pause
         ;(state ((lambda (app (lambda (var ())) '(1 x)))))
         ;(conj
          ;(conj
           ;(== (cdr _.5835) (app (lambda (app (lambda (var ())) '(1 x))) '(y)))
           ;(== _.5837 (1 y)))
          ;(eval-expo _.5835 () (_.5836 . _.5837))))))
      ;(eval-listo _.5778 () _.5780)))
    ;(conj
     ;(disj
      ;(conj
       ;(disj
        ;(conj
         ;(disj
          ;(pause
           ;(state ((lambda (list (car '(1 . _.3225)) 'x))))
           ;(conj
            ;(conj
             ;(conj
              ;(== (app _.5842 _.5843) (lambda (list (car '(1 . _.3225)) 'x)))
              ;(eval-expo _.5842 () (closure _.5846 _.5845)))
             ;(eval-expo _.5843 () _.5844))
            ;(eval-expo _.5846 (_.5844 . _.5845) (closure _.5709 _.5708))))
          ;(pause
           ;(state ((lambda (list (car '(1 . _.3225)) 'x))))
           ;(disj
            ;(conj
             ;(conj
              ;(conj
               ;(== (cons _.5847 _.5848) (lambda (list (car '(1 . _.3225)) 'x)))
               ;(== (_.5849 . _.5850) (closure _.5709 _.5708)))
              ;(eval-expo _.5847 () _.5849))
             ;(eval-expo _.5848 () _.5850))
            ;(disj
             ;(conj
              ;(conj
               ;(== (car _.5851) (lambda (list (car '(1 . _.3225)) 'x)))
               ;(== _.5852 (closure _.5709 _.5708)))
              ;(eval-expo _.5851 () (_.5852 . _.5853)))
             ;(conj
              ;(conj
               ;(== (cdr _.5854) (lambda (list (car '(1 . _.3225)) 'x)))
               ;(== _.5856 (closure _.5709 _.5708)))
              ;(eval-expo _.5854 () (_.5855 . _.5856)))))))
         ;(eval-expo _.5706 () _.5707))
        ;(pause
         ;(state ((lambda (list (car '(1 . _.3225)) 'x))))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (app _.5861 _.5862) '(y))
             ;(eval-expo _.5861 () (closure _.5865 _.5864)))
            ;(eval-expo _.5862 () _.5863))
           ;(eval-expo _.5865 (_.5863 . _.5864) _.5707))
          ;(disj
           ;(conj
            ;(conj
             ;(conj
              ;(== (cons _.5866 _.5867) '(y))
              ;(== (_.5868 . _.5869) _.5707))
             ;(eval-expo _.5866 () _.5868))
            ;(eval-expo _.5867 () _.5869))
           ;(disj
            ;(conj
             ;(conj (== (car _.5870) '(y)) (== _.5871 _.5707))
             ;(eval-expo _.5870 () (_.5871 . _.5872)))
            ;(conj
             ;(conj (== (cdr _.5873) '(y)) (== _.5875 _.5707))
             ;(eval-expo _.5873 () (_.5874 . _.5875))))))))
       ;(eval-expo _.5709 (_.5707 . _.5708) _.5695))
      ;(disj
       ;(pause
        ;(state ((lambda (list (car '(1 . _.3225)) 'x))))
        ;(conj
         ;(conj
          ;(conj
           ;(== (_.6019 . _.6020) ((car '(1 . _.3225)) 'x))
           ;(== (_.6021 . _.6022) (1 y)))
          ;(eval-expo _.6019 ((y)) _.6021))
         ;(eval-listo _.6020 ((y)) _.6022)))
       ;(pause
        ;(state ((lambda (list (car '(1 . _.3225)) 'x))))
        ;(disj
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.5918 _.5919) (list (car '(1 . _.3225)) 'x))
            ;(eval-expo _.5918 ((y)) (closure _.5922 _.5921)))
           ;(eval-expo _.5919 ((y)) _.5920))
          ;(eval-expo _.5922 (_.5920 . _.5921) (1 y)))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (cons _.5923 _.5924) (list (car '(1 . _.3225)) 'x))
             ;(== (_.5925 . _.5926) (1 y)))
            ;(eval-expo _.5923 ((y)) _.5925))
           ;(eval-expo _.5924 ((y)) _.5926))
          ;(disj
           ;(conj
            ;(conj
             ;(== (car _.5927) (list (car '(1 . _.3225)) 'x))
             ;(== _.5928 (1 y)))
            ;(eval-expo _.5927 ((y)) (_.5928 . _.5929)))
           ;(conj
            ;(conj
             ;(== (cdr _.5930) (list (car '(1 . _.3225)) 'x))
             ;(== _.5932 (1 y)))
            ;(eval-expo _.5930 ((y)) (_.5931 . _.5932)))))))))
     ;(eval-listo _.5694 () _.5696)))
   ;(disj
    ;(conj
     ;(disj
      ;(disj
       ;(disj
        ;(pause
         ;(state ((lambda (cons '1 (var ())))))
         ;(conj
          ;(conj
           ;(conj
            ;(== (app _.5960 _.5961) (var ()))
            ;(eval-expo _.5960 ((y)) (closure _.5964 _.5963)))
           ;(eval-expo _.5961 ((y)) _.5962))
          ;(eval-expo _.5964 (_.5962 . _.5963) (y))))
        ;(pause
         ;(state ((lambda (cons '1 (var ())))))
         ;(disj
          ;(conj
           ;(conj
            ;(conj
             ;(== (cons _.5965 _.5966) (var ()))
             ;(== (_.5967 . _.5968) (y)))
            ;(eval-expo _.5965 ((y)) _.5967))
           ;(eval-expo _.5966 ((y)) _.5968))
          ;(disj
           ;(conj
            ;(conj (== (car _.5969) (var ())) (== _.5970 (y)))
            ;(eval-expo _.5969 ((y)) (_.5970 . _.5971)))
           ;(conj
            ;(conj (== (cdr _.5972) (var ())) (== _.5974 (y)))
            ;(eval-expo _.5972 ((y)) (_.5973 . _.5974)))))))
       ;(pause
        ;(state ((lambda (cons '1 (var ())))))
        ;(conj (== (s . _.6028) ()) (lookupo _.6028 () (y)))))
      ;(conj
       ;(pause
        ;(state ((lambda (cons '1 (var ())))))
        ;(conj
         ;(conj (== (cdr _.5911) '1) (== _.5913 1))
         ;(eval-expo _.5911 ((y)) (_.5912 . _.5913))))
       ;(eval-expo _.5810 (_.5680 . _.5681) _.5812)))
     ;(eval-listo _.5671 () _.5673))
    ;(pause
     ;(state ((lambda (cons '1 (var ())))))
     ;(conj
      ;(conj
       ;(conj (== (_.6029 . _.6030) ()) (== (_.6031 . _.6032) ()))
       ;(eval-expo _.6029 () _.6031))
      ;(eval-listo _.6030 () _.6032))))))
