(load "transparent-evalo-transform.scm")

(define (in)
  (define request (read))
  (cond
    ((or (eq? 'good-path request)
         (eq? 'steps-remaining request)
         (and (pair? request)
              (eq? 'jump-to-steps-remaining (car request))))
     request)
    ((pair? request)
     (map (lambda (n)
            (cond
              ((= 0 n) #t)
              ((= 1 n) #f)
              (else (error 'in (format "invalid path segment ~s" n)))))
          request))
    (else (error 'in (format "unexpected request: ~s" request)))))

(define (show ss) (printf "~s\n" (cadr (stream-pretty ss))))

(define (out response)
  (define (bool->bit b) (if b 0 1))
  (define (bools->bits bs) (map bool->bit bs))
  (define output
    (cond
      ((eq? 'good-path (car response)) (bools->bits (cadr response)))
      ((eq? 'follow-path (car response)) (cadr response))
      ((eq? 'steps-remaining (car response)) (map bools->bits (cadr response)))
      (else (error 'out (format "unrecognized output: ~s" response)))))
  (printf "~s\n" output))

(define (read-query/hint) (eval (read)))

(define ss/hint (read-query/hint))
(define hint (car ss/hint))
(define ss (cadr ss/hint))

(interact in show out hint (prune #t ss) #f #t)
