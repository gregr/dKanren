(load "array-mapped-trie.scm")
(load "test.scm")

(define t1 (amt-set amt-empty 3 'a))
(define t2a (amt-set t1 15 'b))
(define t2b (amt-set t1 16 'c))
(define t3a (amt-set t2a 16 'c))
(define t3b (amt-set t2b 15 'b))
(define t4 (amt-set t3a 0 'z))
(define t5a (amt-set t3a 15 'd))
(define t5b (amt-set t4 15 'd))
(define t6 (amt-set t5b 0 'y))

(test 'set-1
  t1
  (amt-leaf 3 'a))
(test 'set-2a
  t2a
  '#(() () () (0 . a) () () () () () () () () () () () (0 . b)))
(test 'set-2b
  t2b
  '#((1 . c) () () (0 . a)))
(test 'set-3a
  t3a
  '#((1 . c) () () (0 . a) () () () () () () () () () () () (0 . b)))
(test 'set-3b
  (equal? t3a t3b)
  #t)
(test 'set-4
  t4
  '#(#((0 . z) (0 . c)) () () (0 . a) () () () () () () () () () () () (0 . b)))

(test 'ref-1
  (amt-leaf-value (amt-ref t1 3))
  'a)
(test 'ref-2
  (amt-leaf-value (amt-ref t2a 3))
  'a)
(test 'ref-3
  (amt-leaf-value (amt-ref t2a 15))
  'b)
(test 'ref-4
  (amt-leaf-value (amt-ref t2b 3))
  'a)
(test 'ref-5
  (amt-leaf-value (amt-ref t2b 16))
  'c)
(test 'ref-6
  (amt-leaf-value (amt-ref t3a 3))
  'a)
(test 'ref-7
  (amt-leaf-value (amt-ref t3a 15))
  'b)
(test 'ref-8
  (amt-leaf-value (amt-ref t3a 16))
  'c)
(test 'ref-9
  (amt-leaf-value (amt-ref t4 3))
  'a)
(test 'ref-10
  (amt-leaf-value (amt-ref t4 15))
  'b)
(test 'ref-11
  (amt-leaf-value (amt-ref t4 16))
  'c)
(test 'ref-12
  (amt-leaf-value (amt-ref t4 0))
  'z)

(test 'ref-missing-1
  (amt-ref t1 2)
  #f)
(test 'ref-missing-2
  (amt-ref t2a 2)
  #f)
(test 'ref-missing-3
  (amt-ref t2b 2)
  #f)
(test 'ref-missing-4
  (amt-ref t3a 2)
  #f)
(test 'ref-missing-5
  (amt-ref t4 2)
  #f)

(test 'set-again-ref-1
  (amt-leaf-value (amt-ref t5a 15))
  'd)
(test 'set-again-ref-2
  (amt-leaf-value (amt-ref t5b 15))
  'd)
(test 'set-again-ref-2
  (amt-leaf-value (amt-ref t6 0))
  'y)
