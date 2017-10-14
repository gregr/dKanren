(load "transparent-evalo-transform.scm")

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

(define (read-query/hint) (eval (read)))

(define ss/hint (read-query/hint))
(define hint (car ss/hint))
(define ss (cadr ss/hint))

(interact in show out hint (prune #t ss) #f #t)
