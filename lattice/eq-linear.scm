(define (list-foldl f acc xs)
  (if (null? xs)
    acc
    (list-foldl f (f (car xs) acc) (cdr xs))))
(define (list-foldr f acc xs)
  (if (null? xs)
    acc
    (f (car xs) (list-foldr f acc (cdr xs)))))


;; General equations
(define eqs-empty '())
(define (eq-size eq) (if (pair? eq) (+ 1 (eq-size (cdr eq))) 0))
(define (eq< a b)
  (cond
    ((not (pair? a)) #f)
    ((= (car a) (car b)) (eq< (cdr a) (cdr b)))
    (else (< (car a) (car b)))))

(define (eq-zero-rhs eq k-rhs k-fail)
  (if (pair? eq)
    (if (= 0 (car eq))
      (eq-zero-rhs (cdr eq) k-rhs k-fail)
      (k-fail eq))
    (k-rhs eq)))

(define (eq-zero? eq)
  (eq-zero-rhs eq (lambda (rhs) (= 0 rhs)) (lambda (_) #f)))

(define (eq-satisfiable? eq)
  (eq-zero-rhs eq (lambda (rhs) (= 0 rhs)) (lambda (_) #t)))

(define (eq-expand eq size)
  (if (pair? eq)
    (cons (car eq) (eq-expand (cdr eq) (- size 1)))
    (append (make-list size 0) eq)))

(define (eq-shrink eq offset cols)
  (if (null? cols) eq
    (let loop ((eq eq) (offset offset) (col (car cols)))
      (if (= offset col)
        (eq-shrink (cdr eq) (+ offset 1) (cdr cols))
        (cons (car eq) (loop (cdr eq) (+ 1 offset) col))))))

(define (eqs-expand eqs size)
  (map (lambda (eq) (eq-expand eq size)) eqs))

(define (eqs-shrink eqs cols)
  (map (lambda (eq) (eq-shrink eq 0 cols)) eqs))

(define (eq-sparse->dense eq)
  (let loop ((eq eq) (idx 0))
    (if (pair? eq)
      (append (make-list (- (caar eq) idx) 0)
              (cons (cdar eq) (loop (cdr eq) (+ 1 (caar eq)))))
      eq)))

(define (eqs-next-solved next) (car next))
(define (eqs-next-eqs next) (cdr next))

;; Linear equations
(define (eqs-linear-add eqs eq)
  (define (eq-linear-simplify a b)
    (let loop ((a a) (b b) (rprefix '()))
      (define (rebuild factor xs)
        (list-foldl (lambda (fst rest) (cons (* factor fst) rest)) xs rprefix))
      (cond
        ((not (pair? a)) (rebuild 1 b))
        ((= 0 (car a)) (loop (cdr a) (cdr b) (cons (car b) rprefix)))
        ((= 0 (car b)) (rebuild 1 b))
        (else
          (let ((fa (car a)) (fb (car b)))
            (let loop ((a (cdr a)) (b (cdr b)) (rprefix '(0)))
              (define (sb a b) (- (* fa b) (* fb a)))
              (if (pair? a)
                (loop (cdr a) (cdr b) (cons (sb (car a) (car b)) rprefix))
                (rebuild fa (list-foldl cons (sb a b) rprefix)))))))))

  (define (eqs-linear-simplify eqs eq)
    (list-foldl (lambda (eq0 eq) (eq-linear-simplify eq0 eq)) eq eqs))

  (define (eqs-linear-insert eqs eq)
    (cond
      ((null? eqs) (list eq))
      ((eq< (car eqs) eq) (cons eq eqs))
      (else (cons (eq-linear-simplify eq (car eqs))
                  (eqs-linear-insert (cdr eqs) eq)))))

  (define (eq-linear-solved eq)
    (let loop ((eq eq) (idx 0))
      (and (pair? eq)
           (if (= 0 (car eq))
             (loop (cdr eq) (+ 1 idx))
             (eq-zero-rhs
               (cdr eq)
               (lambda (rhs) (list (cons idx (/ rhs (car eq)))))
               (lambda (_) '()))))))

  (define (eqs-linear-solved eqs)
    (list-foldr append '() (map eq-linear-solved eqs)))

  (define (eqs-linear-solved-remove eqs)
    (filter (lambda (eq) (null? (eq-linear-solved eq))) eqs))

  (define (eqs-linear-solve eqs)
    (define solved (eqs-linear-solved eqs))
    (if (null? solved) (cons '() eqs)
      (cons solved (eqs-shrink (eqs-linear-solved-remove eqs)
                               (map car solved)))))

  (define size (max (if (null? eqs) 0 (eq-size (car eqs))) (eq-size eq)))
  (define eqs1 (eqs-expand eqs size))
  (define simplified (eqs-linear-simplify eqs1 (eq-expand eq size)))
  (if (eq-zero? simplified)
    (cons '() eqs)
    (and (eq-satisfiable? simplified)
         (eqs-linear-solve (eqs-linear-insert eqs1 simplified)))))
