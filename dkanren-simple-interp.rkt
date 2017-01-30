#lang racket/base
(require
  "dkanren.rkt"
  )

(module+ test
  (require
    rackunit
    )

  (require racket/pretty)
  (define-syntax test
    (syntax-rules ()
     ((_ name expr expected)
      (let ((actual expr))
        (when (not (equal? actual expected))
          (display name)
          (newline)
          (pretty-print actual)
          (newline))
        (check-equal? actual expected)))))

  (define-syntax test-time
    (syntax-rules ()
      ((_ test-name query expected)
        (begin
          (displayln test-name)
          (time (test test-name query expected))))))
  )

(define closure-tag (gensym "#%closure"))

(define (si body)
  `(letrec
     ((eval
        (lambda (expr env)
          (let ((bound? (lambda (sym) (in-env? sym env))))
            (match expr
              (`(,(and 'quote (not (? bound?))) ,(? quotable? datum)) datum)
              (`(,(and 'list (not (? bound?))) . ,a*) (proper-list a* env))
              ((symbol) (lookup expr env))
              (`(,(and op (or (not (symbol)) (? bound?))) ,rand)
                (match (eval op env)
                  (`(,(? ct?) ,x ,body ,env^)
                    (eval body `((,x . ,(eval rand env)) . ,env^)))))
              (`(lambda (,(symbol x)) ,body)
                (list ',closure-tag x body env))))))

      (in-env? (lambda (x env)
                 (match env
                   ('() #f)
                   (`((,a . ,_) . ,d)
                     (or (equal? x a) (in-env? x d))))))

      (lookup (lambda (x env)
                (match env
                  (`((,y . ,v) . ,rest)
                    (if (equal? x y)
                      v
                      (lookup x rest))))))

      (ct? (lambda (datum) (equal? ',closure-tag datum)))

      (quotable?
        (lambda (datum)
          (match datum
            (',closure-tag #f)
            (`(,a . ,d) (and (quotable? a) (quotable? d)))
            (_ #t))))

      (proper-list
        (lambda (expr env)
          (match expr
            ('() '())
            (`(,a . ,d) `(,(eval a env) . ,(proper-list d env)))))))
     ,body))

(define (evalo expr result) (dk-evalo (si `(eval ',expr '())) result))

(module+ test
  (test "quote"
    (run* (q) (evalo '(quote 5) q))
    '((5)))

  (test "list"
    (run* (q) (evalo '(list '5 (list '4 '3)) q))
    '(((5 (4 3)))))

  (test "lambda"
    (run* (q) (evalo '(lambda (x) x) q))
    `(((,closure-tag x x ()))))

  (test "application-1"
    (run* (q) (evalo '((lambda (x) x) '2) q))
    '((2)))

  (test "application-2"
    (run* (q) (evalo '(((lambda (x) (lambda (y) x)) '1) '2) q))
    '((1)))

  (test-time "quine parts"
    (run 1 (q) (evalo
                 `((lambda (x) (list x ,q))
                   (quote
                     (lambda (x) (list x ,q))))
                 `((lambda (x) (list x ,q))
                   (quote
                     (lambda (x) (list x ,q))))))
    '(((list (quote quote) x))))

  (test-time "quine more parts"
    (run 1 (q) (evalo
                 `((lambda (x) ,q)
                   (quote (lambda (x) ,q)))
                 `((lambda (x) ,q)
                   (quote (lambda (x) ,q)))))
    '(((list x (list (quote quote) x)))))
  )
