(load "transparent-evalo.scm")

(define (q-transform f inputs)
  (query (defn)
         (fresh (body)
           (== `(lambda ,body) defn)
           (evalo `(list . ,(map (lambda (input) `(app ,defn ',input)) inputs))
                  (map f inputs)))))

(define (q-transform-hint f inputs hint)
  (query (defn)
         (== hint defn)
         (evalo `(list . ,(map (lambda (input) `(app ,defn ',input)) inputs))
                (map f inputs))))

(define (q-np n) (q-transform (lambda (x) (cons n x)) '((x) (y))))

(define q-quine (query (p) (evalo p p)))


;(define (in)
  ;(printf "enter a path: ")
  ;(read))

;(define (show ss)
  ;(pretty-print (or (cadr (stream-pretty ss)) ss))
  ;(newline))

;(define (out flag) (printf "result: ~s\n" flag))

;(define ss (simplify (query (x)
                       ;(conde
                         ;((== 1 3))
                         ;((== 2 x) (== 3 x))
                         ;((conde
                            ;((== 3 x) (== 4 x))
                            ;((== 5 x)))
                          ;(conde
                            ;((== 1 1))
                            ;((== #t #t)))
                          ;(conde
                            ;((== #f #t))
                            ;((== #t #t))))))))

(define (in)
  (define request (read))
  (cond
    ((eq? 'good-path request) 'good-path)
    ((pair? request)
     (map (lambda (n)
            (cond
              ((= 0 n) #t)
              ((= 1 n) #f)
              (else (error 'in (format "invalid path segment ~s" n)))))
          request))
    (else request)))

(define (show ss) (printf "~s\n" (cadr (stream-pretty ss))))

(define (out response)
  (define output
    (cond
      ((eq? 'good-path (car response))
       (map (lambda (b) (if b 0 1)) (cadr response)))
      ((eq? 'follow-path (car response)) (cadr response))
      (else (error 'out (format "unrecognized output: ~s" response)))))
  (printf "~s\n" output))

(define (read-query) (simplify (eval (read))))

(define ss (read-query))

(interact in show out ss ss #f #t)