;; M[n, k] = min{i from 1 to n} max(M[i, k - 1], sum(s_j for j from i+1 to n)
;; M[1, k] = s1, for all k > 0
;; M[n, 1] = sum(s_i for i from 1 to n)

(define-syntax iter
  (syntax-rules ()
    ((_ i start end expr)
     (map (lambda (i) expr) (range start (+ end 1))))))

(define (min* ns) (apply min ns))
(define (sum n*) (foldl + 0 n*))

(define (M n k s*)
  (cond ((= 1 n) (car s*))
        ((= 1 k) (sum (take s* n)))
        (else (min* (iter i 1 n (max (M i (- k 1) s*) (sum (drop (take s* n) i))))))))

(define (integer-partition k s*) (M (length s*) k s*))

(displayln (integer-partition 3 '(100 200 300 400 500 600 700 800 900)))
