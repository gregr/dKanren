;; Ideally, this implementation would assume (fixnum? index) is always #t, but
;; load-only portability is challenging at the moment.  So use makeshift
;; definitions in place of fixnum operations for now.
;; fx=, fx<, fx+, fx-, fxmax, bitwise-and: fxand
;; Racket: (require racket/fixnum), nsl: fxlshift, nsr: fxrshift
;; Chez: nsl: fxsll, nsr: fxsra

(define shift-size 4)

(define (nsl n sz) (* n (expt 2 sz)))
(define (nsr n sz) (floor (/ n (expt 2 sz))))

(define amt-branch-size (nsl 1 shift-size))
(define local-mask (- amt-branch-size 1))
(define (shift index) (nsr index shift-size))
(define (unshift index) (nsl index shift-size))
(define (local index) (bitwise-and index local-mask))

(define amt-empty '())
(define (amt-empty? amt) (null? amt))
(define (amt-leaf index value) (cons index value))
(define (amt-leaf? amt) (pair? amt))
(define (amt-leaf-index leaf) (car leaf))
(define (amt-leaf-value leaf) (cdr leaf))
(define (amt-branch? amt) (vector? amt))

(define (amt-branch-new i0 v0)
  (define result (make-vector (+ i0 1) amt-empty))
  (vector-set! result i0 v0)
  result)
(define (amt-branch-resize branch len)
  (define len0 (vector-length branch))
  (if (= len len0) (vector-copy branch)
    (let ((result (make-vector len amt-empty)))
      (let copy ((ci (- (min len0 len) 1)))
        (if (> 0 ci) result
          (begin (vector-set! result ci (vector-ref branch ci))
                 (copy (- ci 1))))))))
(define (amt-branch-ref branch idx)
  (if (< idx (vector-length branch)) (vector-ref branch idx) amt-empty))
(define (amt-branch-set branch idx val)
  (define result (amt-branch-resize branch (max (vector-length branch)
                                                (+ idx 1))))
  (vector-set! result idx val)
  result)
(define (amt-branch-remove branch idx)
  (define (size<=? size idx)
    (cond ((> 0 idx) #t)
          ((amt-empty? (vector-ref branch idx)) (size<=? size (- idx 1)))
          ((< 0 size) (size<=? (- size 1) (- idx 1)))
          (else #f)))
  (define (leaf-or-len size len)
    (define idx (- len 1))
    (define amt (vector-ref branch idx))
    (if (and (amt-leaf? amt) (size<=? size (- idx 1)))
      (amt-leaf (+ idx (unshift (amt-leaf-index amt)))
                (amt-leaf-value amt))
      len))
  (define len0 (vector-length branch))
  (define len1-or-amt
    (if (< (+ 1 idx) len0) (leaf-or-len 1 len0)
      (let last-idx ((li (- idx 1)))
        (cond
          ((> 0 li) amt-empty)
          ((amt-empty? (vector-ref branch li)) (last-idx (- li 1)))
          (else (leaf-or-len 0 (+ li 1)))))))
  (if (number? len1-or-amt)
    (let ((result (amt-branch-resize branch len1-or-amt)))
      (when (< idx len1-or-amt) (vector-set! result idx amt-empty))
      result)
    len1-or-amt))

(define (amt-size amt)
  (cond
    ((amt-branch? amt)
     (let loop ((ci 0) (sz 0))
       (if (= amt-branch-size ci) sz
         (loop (+ ci 1) (+ sz (amt-size (amt-branch-ref amt ci)))))))
    ((amt-leaf? amt) 1)
    (else 0)))

(define (amt-ref amt index)
  (cond
    ((amt-branch? amt) (amt-ref (amt-branch-ref amt (local index))
                                (shift index)))
    ((amt-leaf? amt) (and (= index (amt-leaf-index amt)) amt))
    (else #f)))

(define (amt-set amt index val)
  (cond
    ((amt-branch? amt)
     (let ((li (local index)))
       (amt-branch-set
         amt li (amt-set (amt-branch-ref amt li) (shift index) val))))
    ((amt-leaf? amt)
     (let ((index0 (amt-leaf-index amt)))
       (if (= index0 index) (amt-leaf index val)
         (amt-set (amt-branch-new (local index0)
                                  (amt-leaf (shift index0) (amt-leaf-value amt)))
                  index val))))
    (else (amt-leaf index val))))

(define (amt-remove amt index)
  (cond
    ((amt-branch? amt)
     (let* ((li (local index))
            (child0 (amt-branch-ref amt li))
            (child1 (amt-remove child0 (shift index))))
       (cond
         ((eq? child0 child1) amt)
         ((amt-empty? child1) (amt-branch-remove amt li))
         (else (amt-branch-set amt li child1)))))
    ((amt-leaf? amt) (if (= index (amt-leaf-index amt)) amt-empty amt))
    (else amt-empty)))
