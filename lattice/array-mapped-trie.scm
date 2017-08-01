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
(define (amt-branch-ref branch idx)
  (if (< idx (vector-length branch)) (vector-ref branch idx) amt-empty))
(define (amt-branch-set branch idx val)
  (define len0 (vector-length branch))
  (define len1 (max len0 (+ idx 1)))
  (define result (make-vector len1 amt-empty))
  (let copy ((ci 0))
    (if (= len0 ci) (begin (vector-set! result idx val) result)
      (begin (vector-set! result ci (vector-ref branch ci)) (copy (+ ci 1))))))

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
