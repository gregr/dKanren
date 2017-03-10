#lang racket/base
(provide
  ==
  =/=
  absento
  dk-evalo
  fail
  fresh
  not-numbero
  not-pairo
  not-symbolo
  numbero
  symbolo
  run
  run*
  succeed
  term?
  )

(require
  racket/control
  racket/list
  racket/match
  racket/vector
  )

(module+ test
  (require
    rackunit
    ))

; TODO:
; pattern compilation
;   don't forget to wrap ps->index result with outer guessing list
;   add domains (and sub-domains for pairs) to guessing lists, bottom up
;   new pattern matching procedure
;     follow full indices for known values
;     otherwise
;       try each guessing list entry
;       use rhs patterns only for ambiguity pruning, not indexing
;       domainify upon ambiguity
;   fix =/= svs, which don't really work at the moment
;   possibly improve pattern assertion definitions
;   manual low level conde-like match construction

; force remaining goals that are mentioned only in vattrs (e.g. disunify-or-suspend)
; unlike normal mk, all vars in =/=* should be tracked for earliest access to determinism
;   these constraints can shrink domains, which may trigger new unifications, and so on
; implement quasiquote, let, let*, and, or, cond, match for evalo
; extended mk variant, mixing in deterministic computation where possible
;   tagging and pluggable solvers (one in particular)
;     aggressive, parallel term guesser
;       not complete on its own, but likely more effective for typical synthesis
;       analayzes all eval goals for the same term, analyzing the different envs and results
;       size-incrementing basic component enumeration
;         literals, vars, car/cdrs of everything available in env
;         then cons (only as backwards info flow suggests) and predicate applications of components
;         note: some primitives may have been shadowed or renamed
;         may introduce lambdas when backwards info flow indicates a closure is necessary
;       identifying components useful as conditions (capable of partitioning the eval goals)
;         not useful unless component expresses both #f and not #f at least once each across envs
;         e.g. literals are not useful
;         could perform this identification in parallel, for each component over each env
;           defer components that are not available in all envs, until conditional splitting
;       general size-incrementing component enumeration
;         e.g. applying non-primitive procedures (try to avoid unnested self-application/non-termination)
;              conditional splitting, producing lambdas, letrecs, etc.
;         conditional splitting (if, cond, match, depending on shadowing/complexity)
;           if none of if/cond/match are available, and no closures are capable of conditional splitting,
;           could potentially refute this line of search early
;           if there are basic components useful as conditions, try those before general components
; tagging and reification for =/=, absento
;   could do this just for =/= and be satisfied with some infinite absento enumerations
; port everything to chez
; performance and benchmarking
;   move from closure-encoding interpreter to ahead-of-time compiler
;   tweak mk data reprs
;     e.g. try path compression
;   try relational arithmetic
;   try first-order minicurry tests
; future
;   de bruijn encoding interpreter
;     could be a useful alternative to closure-encoding for 1st order languages
;     try for both dkanren (for comparison) and relational interpreter (may improve perf)
; possible ways to improve performance
;   infer impossible scrutinee domains and immediately distypify them (integrate with prefix factoring?)
;   pattern prefix factoring (also affects run* termination)
;     ideally we'd use full-blown pattern compilation
;       but hard to preserve programmer's intended(?) clause search priority with nesting changes
;       in contrast factoring only the final clauses will not affect priorities (since nothing follows them)
;   match/r, reversible match: manually describe a reversed computation
;     should be much better than falling back to denote-rhs-pattern-unknown

; profiling results
; quine:
;   match-chain-try loop: 58.6%, 27.0%
;   pattern-exec-and: 54.2%, 2.1%
; thrine:
;   match-chain-try loop: 84.3%, 23.1%
;   pattern-exec-and: 77.5%, 4.6%
;   pattern-assert-==: 32.1%, 21.4%
; append (cdr xs):
;   state-resume-det loop1 94.6%, 0%
;   match-chain-try loop: 79.7%, 45.5%
; append more:
;   match-chain-try loop: 92.1%, 75.8%
; arithmetic:
;   retry: 73.6%, 1.4%
;   match-chain-try loop: 69.3%, 16.3%
;   pattern-exec-and: 42.8%, 13.3%
;   pattern-assert-or: 31.8%, 1.4%
;   pattern-assert-==: 28.5%, 12.3%
;   pattern-assert-pair: 27.6%, 0%
;   unify: 17.6%, 10.8%
;   val-depend: 6.8%

(define-syntax defrec
  (syntax-rules ()
    ((_ name fnames ...) (struct name (fnames ...) #:transparent))))

(define-syntax let*/and
  (syntax-rules ()
    ((_ () rest ...) (and rest ...))
    ((_ ((name expr) ne* ...) rest ...)
     (let ((name expr))
       (and name (let*/and (ne* ...) rest ...))))))

(define-syntax let/if
  (syntax-rules ()
    ((_ (name expr) true-alt false-alt)
     (let ((name expr)) (if name true-alt false-alt)))))

(define-syntax let/list
  (syntax-rules ()
    ((_ loop ((ncar ncdr expr) binding ...) pair-alt else-alt)
     (let loop ((npair expr) binding ...)
       (match npair
         (`(,ncar . ,ncdr) pair-alt)
         ('() else-alt))))))

(define store-empty  (hasheq))
(define store-ref    hash-ref)
(define store-set    hash-set)
(define store-remove hash-remove)
(define store-keys hash-keys)
(define (list-add-unique xs v) (if (memq v xs) xs (cons v xs)))
(define (list-cons-unique x xs) (if (memq x xs) xs (cons x xs)))
(define (list-append-unique xs ys)
  (if (null? xs) ys
    (let ((zs (list-append-unique (cdr xs) ys))
          (x0 (car xs)))
      (if (memq x0 ys) zs (cons x0 zs)))))
(define (list-subtract xs ys)
  (if (null? xs) xs
    (let ((x0 (car xs))
          (xs1 (list-subtract (cdr xs) ys)))
     (if (memq x0 ys) xs1
       (if (eq? xs1 (cdr xs)) xs
         (cons x0 xs1))))))

(defrec var name)
(define var-0 (var 'initial))
(define (value->vars st val)
  (match (walk1 st val)
    (`(,a . ,d) (list-append-unique (value->vars st a) (value->vars st d)))
    ((? var? vr) (list vr))
    (_ '())))

(define domain-full '(pair symbol number () #f #t))
(define (domain-remove dmn type) (remove type dmn))
(define (domain-has-type? dmn type)
  (or (eq? domain-full dmn) (memq type dmn)))
(define (domain-has-val? dmn val)
  (or (eq? domain-full dmn) (memq (match val
                                    ((? pair?) 'pair)
                                    ((? symbol?) 'symbol)
                                    ((? number?) 'number)
                                    (_ val)) dmn)))
(define (domain-overlap? d1 d2)
  (cond ((eq? domain-full d1) #t)
        ((eq? domain-full d2) #t)
        (else (let loop ((d1 d1) (d2 d2))
                (or (memq (car d1) d2)
                    (and (pair? (cdr d1)) (loop (cdr d1) d2)))))))
(define (domain-intersect d1 d2)
  (cond ((eq? domain-full d1) d2)
        ((eq? domain-full d2) d1)
        (else (let loop ((d1 d1) (d2 d2) (di '()))
                (let* ((d1a (car d1))
                       (d1d (cdr d1))
                       (d2d (memq d1a d2))
                       (di (if d2d (cons (car d1) di) di))
                       (d2d (if d2d (cdr d2d) d2)))
                  (if (or (null? d1d) (null? d2d))
                    (if (null? di) #f (reverse di))
                    (loop d1d d2d di)))))))

(defrec vattr domain =/=s goals dependencies)
(define (vattrs-get vs vr) (store-ref vs vr vattr-empty))
(define vattrs-set store-set)
(define vattr-empty (vattr domain-full '() '() '()))
(define (vattr-domain-set va dmn)
  (vattr dmn (vattr-=/=s va) (vattr-goals va) (vattr-dependencies va)))
(define (vattr-=/=s-clear va)
  (vattr (vattr-domain va) '() (vattr-goals va) (vattr-dependencies va)))
(define (state-vattr-=/=s-add st vr va val)
  (if (or (var? val) (domain-has-val? (vattr-domain va) val))
    (let ((=/=s (vattr-=/=s va)))
      (if (memq val =/=s) st
        (state-var-set
          st vr (vattr (vattr-domain va)
                       (cons val =/=s)
                       (vattr-goals va)
                       (vattr-dependencies va)))))
    st))
(define (vattr-=/=s-add va val)
  (if (or (var? val) (domain-has-val? (vattr-domain va) val))
    (let ((=/=s (vattr-=/=s va)))
      (if (memq val =/=s) va
        (vattr (vattr-domain va)
               (cons val =/=s)
               (vattr-goals va)
               (vattr-dependencies va))))
    va))
(define (vattr-=/=s-has? va val) (memq val (vattr-=/=s va)))
(define (vattr-associate va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (list-cons-unique goal (vattr-goals va))
         (vattr-dependencies va)))
(define (vattr-associate* va goals)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (list-append-unique goals (vattr-goals va))
         (vattr-dependencies va)))
(define (vattr-depend va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (list-cons-unique goal (vattr-goals va))
         (list-cons-unique goal (vattr-dependencies va))))
(define (vattr-depend* va goals)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (list-append-unique goals (vattr-goals va))
         (list-append-unique goals (vattr-dependencies va))))
(define (vattr-goals-clear va)
  (vattr (vattr-domain va) (vattr-=/=s va) '() (vattr-dependencies va)))
(define (vattr-dependencies-clear va)
  (vattr (vattr-domain va) (vattr-=/=s va) (vattr-goals va) '()))
(define (vattr-overlap? va1 va2)
  (domain-overlap? (vattr-domain va1) (vattr-domain va2)))
(define (vattr-intersect va1 va2)
  (let*/and ((di (domain-intersect (vattr-domain va1) (vattr-domain va2))))
    (vattr di
           (list-append-unique (vattr-=/=s va1) (vattr-=/=s va2))
           (append (vattr-goals va1) (vattr-goals va2))
           (append (vattr-dependencies va1)
                   (vattr-dependencies va2)))))

(defrec goal-suspended tag result blockers retry guess active?)
(define (goal-ref-new) (gensym))
(define (goal-retry goals goal)
  (if (procedure? goal) goal
    (let*/and ((gsusp (store-ref goals goal #f)))
      (goal-suspended-retry gsusp))))

(define (goal-block-cons block blocks)
  (if (null? block) blocks (cons block blocks)))
(defrec schedule det nondet)
(define schedule-empty (schedule '() '()))
(define (schedule-add-det sch det)
  (schedule (goal-block-cons det (schedule-det sch))
            (schedule-nondet sch)))
(define (schedule-add-nondet sch nondet)
  (schedule (schedule-det sch)
            (goal-block-cons nondet (schedule-nondet sch))))
(define (schedule-add sch det nondet)
  (schedule (goal-block-cons det (schedule-det sch))
            (goal-block-cons nondet (schedule-nondet sch))))
(define (schedule-clear-det sch)
  (schedule '() (schedule-nondet sch)))

(defrec state vs goals schedule)
(define state-empty (state store-empty store-empty schedule-empty))
(define (state-var-get st vr) (vattrs-get (state-vs st) vr))
(define (state-var-set st vr va)
  (state (vattrs-set (state-vs st) vr va)
         (state-goals st)
         (state-schedule st)))
(define (state-suspend st forward? vr goal)
  (state-var-set
    st vr (let ((va (state-var-get st vr)))
            (if forward?
              (vattr-associate va goal)
              (vattr-depend va goal)))))
(define (state-suspend* st var-forwards var-backwards goal-ref goal)
  (define (update vattr-add vs vrs)
    (foldl (lambda (vr vs)
             (vattrs-set vs vr (vattr-add (vattrs-get vs vr) goal-ref)))
           vs vrs))
  (let* ((goals (store-set (state-goals st) goal-ref goal))
         (vs (state-vs st))
         (vs (update vattr-associate vs var-forwards) )
         (active? (and (goal-suspended? goal) (goal-suspended-active? goal)))
         (vs (update
               (if active? vattr-depend vattr-associate) vs var-backwards)))
    (state vs goals (let ((sch (state-schedule st)))
                      (if (and (null? var-backwards) active?)
                        (schedule-add-nondet sch (list goal-ref))
                        sch)))))
(define (state-goals-set st goals)
  (state (state-vs st) goals (state-schedule st)))
(define (state-remove-goal st goal)
  (state-goals-set st (store-remove (state-goals st) goal)))
(define (state-schedule-clear-det st)
  (state (state-vs st) (state-goals st) (schedule-clear-det
                                          (state-schedule st))))
(define (state-schedule-set st sch) (state (state-vs st) (state-goals st) sch))
(define (state-schedule-clear st) (state-schedule-set schedule-empty))

(define det-quota 200)
(define det-cost 0)
(define (det-reset) (set! det-cost 0))
(define (det-pay cost)
  (let ((next-cost (+ cost det-cost)))
    (if (> det-quota next-cost)
      (set! det-cost next-cost)
      (begin (set! det-cost 0) (shift k k)))))
(define-syntax reset-cost
  (syntax-rules ()
    ((_ body ...) (let ((result (reset body ...)))
                    (det-reset)
                    result))))

(define (state-resume-det1 st)
  (let* ((det (schedule-det (state-schedule st)))
         (st (state-schedule-clear-det st)))
    (let/list loop ((goals det det) (st st))
      (let/list loop1 ((goal goals goals) (st st))
        (let/if (retry (goal-retry (state-goals st) goal))
          (let*/and ((st (retry st))
                     (st (state-resume-det1 st)))
            (loop1 goals st))
          (loop1 goals st))
        (loop det st))
      st)))
(define (state-resume-nondet1 st)
  (let* ((sgoals (state-goals st))
         (nondet (schedule-nondet (state-schedule st))))
    (let/list loop ((goals nondet (reverse nondet)) (vs (state-vs st)))
      (let/list loop1 ((goal goals1 goals))
        (let/if (gsusp (store-ref sgoals goal #f))
          (let/list loop2 ((blocker blockers (goal-suspended-blockers gsusp)))
            (let-values (((blocker va) (walk-vs vs blocker)))
              (let ((deps (vattr-dependencies va)))
                (if (null? deps) (loop2 blockers)
                  (loop
                    (cons deps (cons (cons goal goals1) nondet))
                    (vattrs-set vs blocker (vattr-dependencies-clear va))))))
            (let ((sch-next (schedule '() (reverse (cons (cons goal goals1) nondet)))))
              (bind
                (bind ((goal-suspended-guess gsusp)
                       (state vs sgoals schedule-empty))
                      state-resume-pending)
                (lambda (st)
                  (state-resume-nondet1 (state-schedule-set st sch-next))))))
          (loop1 goals1))
        (loop nondet vs))
      (state vs sgoals schedule-empty))))
(define (state-resume-pending st)
  (bind (reset-cost (state-resume-det1 st)) state-resume-nondet1))
(define (state-resume-remaining st)
  (define sgoals (state-goals st))
  (define all (store-keys sgoals))
  (let/list loop ((goal-ref goal-refs all) (active '()))
    (let ((gsusp (store-ref sgoals goal-ref)))
      (if (goal-suspended-active? gsusp)
        (loop goal-refs (cons goal-ref active))
        (loop goal-refs active)))
    (if (null? active)
      (if (null? all) st
        ; TODO: also force remaining unnamed goals from vattrs
        (bind
          (state-resume-nondet1
            (state-schedule-set st (schedule '() (list all))))
          state-resume-remaining))
      (bind (state-resume-nondet1
              (state-schedule-set st (schedule '() (list active))))
            state-resume-remaining))))
(define (state-resume st)
  (bind (state-resume-pending st) state-resume-remaining))

(define (state-var-domain-set st vr va dmn)
  (match dmn
    (`(,(and (not 'symbol) (not 'number) singleton))
      (state-var-== st vr va (if (eq? 'pair singleton)
                               `(,(var 'pair-a) . ,(var 'pair-d)) singleton)))
    ('() #f)
    (dmn (let ((st (state-var-set st vr (vattr-domain-set va dmn))))
           (if (eq? domain-full dmn) st
             (state (state-vs st)
                    (state-goals st)
                    (schedule-add (state-schedule st)
                                  (vattr-goals va)
                                  (vattr-dependencies va))))))))
(define (state-var-domain-== st vr va dmn)
  (let*/and ((dmn (domain-intersect (vattr-domain va) dmn)))
    (state-var-domain-set st vr va dmn)))
(define (state-var-type-== st vr va type)
  (and (domain-has-type? (vattr-domain va) type)
       (state-var-domain-set st vr va `(,type))))
(define (state-var-type-=/= st vr va type)
  (state-var-domain-set st vr va (domain-remove (vattr-domain va) type)))

;; Ideally we would notify any vars in =/=s that they can drop 'vr' from their
;; own =/=s, but not doing so shouldn't be a big deal.  Same story when
;; handling state-var-==-var.
(define (state-var-== st vr va val)
  (cond
    ((eq? vattr-empty va) (state-var-set st vr val))
    ((domain-has-val? (vattr-domain va) val)
     (let ((=/=s (vattr-=/=s va)))
       (and (not (memq val =/=s))
            (let* ((vps (filter (if (pair? val)
                                  (lambda (x) (or (var? x) (pair? x)))
                                  var?) =/=s))
                   (deps (vattr-dependencies va))
                   (vs (if (null? deps) (state-vs st)
                         (let val-depend ((vs (state-vs st)) (val val))
                           (match val
                             (`(,pa . ,pd) (val-depend (val-depend vs pa) pd))
                             ((? var? evr)
                              (let-values (((evr eva) (walk-vs vs evr)))
                                (if (var? evr)
                                  (store-set vs evr (vattr-depend* eva deps))
                                  (val-depend vs evr))))
                             (_ vs)))))
                   (st (state (store-set vs vr val)
                              (state-goals st)
                              (schedule-add (state-schedule st)
                                            (vattr-goals va)
                                            deps))))
              (disunify* st val vps)))))
    (else #f)))
(define (state-var-=/= st vr va val)
  (if (or (eq? '() val) (eq? #f val) (eq? #t val))
    (state-var-type-=/= st vr va val)
    (state-vattr-=/=s-add st vr va val)))
(define (state-var-==-var st vr1 va1 vr2 va2)
  (let*/and ((va (vattr-intersect va1 va2)))
    (let* ((=/=s (vattr-=/=s va))
           (va (vattr-=/=s-clear va))
           (st (state-var-set st vr1 vr2))
           (st (state (state-vs st)
                      (state-goals st)
                      (schedule-add-det (state-schedule st) (vattr-goals va))))
           (va (vattr-goals-clear va)))
      (disunify* (state-var-set st vr2 va) vr2 =/=s))))
(define (state-var-=/=-var st vr1 va1 vr2 va2)
  (if (vattr-overlap? va1 va2)
    (state-vattr-=/=s-add (state-vattr-=/=s-add st vr1 va1 vr2) vr2 va2 vr1)
    st))
(define (state-var-=/=-redundant? st vr va val)
  (or (not (domain-has-val? (vattr-domain va) val))
      (vattr-=/=s-has? va val)))
(define (state-var-=/=-var-redundant? st vr1 va1 vr2 va2)
  (or (not (vattr-overlap? va1 va2))
      (vattr-=/=s-has? va1 vr2)
      (vattr-=/=s-has? va2 vr1)))

(define (walk-vs vs vr)
  (let ((va (vattrs-get vs vr)))
    (cond ((vattr? va) (values vr va))
          ((var? va) (walk-vs vs va))
          (else (values va #f)))))
(define (walk st tm)
  (if (var? tm)
    (walk-vs (state-vs st) tm)
    (values tm #f)))
(define (walk1 st tm) (let-values (((val va) (walk st tm))) val))
(define (not-occurs? st vr tm)
  (if (pair? tm) (let-values (((ht _) (walk st (car tm))))
                   (let*/and ((st (not-occurs? st vr ht)))
                     (let-values (((tt _) (walk st (cdr tm))))
                               (not-occurs? st vr tt))))
    (if (eq? vr tm) #f st)))
(define (unify st v1 v2)
  (let-values (((v1 va1) (walk st v1))
               ((v2 va2) (walk st v2)))
    (cond ((eq? v1 v2) st)
          ((var? v1) (if (var? v2)
                       (state-var-==-var st v1 va1 v2 va2)
                       (and (not-occurs? st v1 v2)
                            (state-var-== st v1 va1 v2))))
          ((var? v2) (and (not-occurs? st v2 v1)
                          (state-var-== st v2 va2 v1)))
          ((and (pair? v1) (pair? v2))
           (let*/and ((st (unify st (car v1) (car v2))))
             (unify st (cdr v1) (cdr v2))))
          (else #f))))
(define (disunify* st v1 vs)
  (if (null? vs) st (let*/and ((st (disunify st v1 (car vs))))
                      (disunify* st v1 (cdr vs)))))
(define (disunify st v1 v2) (disunify-or st v1 v2 '()))

(define (disunify-or-suspend st v1 va1 v2 pairings)
  (state-suspend st #t v1 (lambda (st) (disunify-or st v1 v2 pairings))))
(define (disunify-or st v1 v2 pairings)
  (let-values (((v1 va1) (walk st v1)))
    (disunify-or-rhs st v1 va1 v2 pairings)))
(define (disunify-or-rhs st v1 va1 v2 pairings)
  (let-values (((v2 va2) (walk st v2)))
    (cond
      ((eq? v1 v2)
       (and (pair? pairings)
            (disunify-or st (caar pairings) (cdar pairings) (cdr pairings))))
      ((var? v1)
       (if (null? pairings)
         (if (var? v2)
           (state-var-=/=-var st v1 va1 v2 va2)
           (state-var-=/= st v1 va1 v2))
         (if (var? v2)
           (if (state-var-=/=-var-redundant? st v1 va1 v2 va2) st
             (if (state-var-=/=-var st v1 va1 v2 va2)
               (disunify-or-suspend st v1 va1 v2 pairings)
               (disunify-or
                 st (caar pairings) (cdar pairings) (cdr pairings))))
           (if (state-var-=/=-redundant? st v1 va1 v2) st
             (if (state-var-=/= st v1 va1 v2)
               (disunify-or-suspend st v1 va1 v2 pairings)
               (disunify-or
                 st (caar pairings) (cdar pairings) (cdr pairings)))))))
      ((var? v2)
       (if (null? pairings)
         (state-var-=/= st v2 va2 v1)
         (if (state-var-=/=-redundant? st v2 va2 v1) st
           (if (state-var-=/= st v2 va2 v1)
             (disunify-or-suspend st v2 va2 v1 pairings)
             (disunify-or
               st (caar pairings) (cdar pairings) (cdr pairings))))))
      ((and (pair? v1) (pair? v2))
       (disunify-or
         st (car v1) (car v2) (cons (cons (cdr v1) (cdr v2)) pairings)))
      (else st))))

(define (typify st type val)
  (let-values (((val va) (walk st val)))
    (if (var? val) (state-var-type-== st val va type)
      (and (match type
             ('symbol (symbol? val))
             ('number (number? val))
             ('pair (pair? val))
             (_ (eq? type val)))
           st))))
(define (distypify st type val)
  (let-values (((val va) (walk st val)))
    (if (var? val) (state-var-type-=/= st val va type)
      (and (not (match type
                  ('symbol (symbol? val))
                  ('number (number? val))
                  ('pair (pair? val))
                  (_ (eq? type val))))
           st))))

(define (domainify st val dmn)
  (let-values (((val va) (walk st val)))
    (if (var? val)
      (state-var-domain-== st val va dmn)
      (and (domain-has-val? dmn val) st))))

(define (succeed st) st)
(define (fail st) #f)

(define (== t0 t1) (lambda (st) (unify st t0 t1)))
(define (=/= t0 t1) (lambda (st) (disunify st t0 t1)))
(define (symbolo tm) (lambda (st) (typify st 'symbol tm)))
(define (numbero tm) (lambda (st) (typify st 'number tm)))
(define (not-symbolo tm) (lambda (st) (distypify st 'symbol tm)))
(define (not-numbero tm) (lambda (st) (distypify st 'number tm)))
(define (not-pairo tm) (lambda (st) (distypify st 'pair tm)))

(define (absento atom tm)
  (dk-evalo
    `(letrec ((absent?
                (lambda (tm)
                  (match/lazy tm
                    (`(,ta . ,td) (and (absent? ta) (absent? td)))
                    (',atom #f)
                    (_ #t)))))
       (absent? ',tm))
    #t))

(define-syntax zzz (syntax-rules () ((_ body ...) (lambda () body ...))))

(define (mplus ss zss)
  (match ss
    (#f (zss))
    ((? procedure?) (zzz (mplus (zss) ss)))
    ((? state?) (cons ss zss))
    (`(,result . ,zss1) (cons result (zzz (mplus (zss) zss1))))))

(define (take n ss)
  (if (and n (zero? n)) '()
    (match ss
      (#f '())
      ((? procedure?) (take n (ss)))
      ((? state?) (list ss))
      (`(,result . ,ss) (cons result (take (and n (- n 1)) ss))))))

(define (bind ss goal)
  (match ss
    (#f #f)
    ((? procedure?) (zzz (bind (ss) goal)))
    ((? state?) (goal ss))
    (`(,result . ,zss) (mplus (goal result) (zzz (bind (zss) goal))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e goal0 goal ...) (bind* (bind e goal0) goal ...))))

(define-syntax let/vars
  (syntax-rules ()
    ((_ (vname ...) body ...)
     (let ((vname (var (gensym (symbol->string 'vname)))) ...) body ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (vname ...) goal ...)
     (let/vars (vname ...) (lambda (st) (bind* st goal ...))))))

(define (reify-var ix) (string->symbol (string-append "_." (number->string ix))))
(define (reify tm)
  (lambda (st)
    (let-values
      (((st ixs tm)
        (let loop ((st st) (ixs store-empty) (tm tm))
          (let-values (((tm va) (walk st tm)))
            (cond
              ((var? tm)
               (let/if (ix (store-ref ixs tm #f))
                 (values st ixs (reify-var ix))
                 (let ((ix (hash-count ixs)))
                   (values st (store-set ixs tm ix) (reify-var ix)))))
              ((pair? tm)
               (let*-values (((st ixs thd) (loop st ixs (car tm)))
                             ((st ixs ttl) (loop st ixs (cdr tm))))
                 (values st ixs (cons thd ttl))))
              (else (values st ixs tm)))))))
      tm)))

(define-syntax run
  (syntax-rules ()
    ((_ n (qv ...) goal ...)
     (map (reify var-0)
          (take n (zzz ((fresh (qv ...)
                          (== (list qv ...) var-0) goal ... state-resume)
                        state-empty)))))))
(define-syntax run* (syntax-rules () ((_ body ...) (run #f body ...))))


(define (quotable? v)
  (match v
    (`(,a . ,d) (and (quotable? a) (quotable? d)))
    ((? procedure?) #f)
    (_ #t)))

(define (rev-append xs ys)
  (if (null? xs) ys (rev-append (cdr xs) (cons (car xs) ys))))

(define-syntax let*/state
  (syntax-rules ()
    ((_ () body ...) (begin body ...))
    ((_ (((st val) expr) bindings ...) body ...)
     (let-values (((st val) expr))
       (if st
         (let*/state (bindings ...) body ...)
         (values #f #f))))))

(define (goal-value val) (lambda (st) (values st val)))
(define (denote-value val) (lambda (env) (goal-value val)))
(define denote-true (denote-value #t))
(define denote-false (denote-value #f))
(define (denote-lambda params body senv)
  (match params
    ((and (not (? symbol?)) params)
     (let ((dbody (denote-term body (extend-env* params params senv)))
           (nparams (length params)))
       (lambda (env)
         (goal-value
           (lambda args
             (let ((nargs (length args)))
               (when (not (= nparams nargs))
                 (error
                   (format
                     "expected ~a args, given ~a; params=~v, body=~v, args=~v"
                     nparams nargs params body args))))
             (dbody (rev-append args env)))))))
    (sym
      (let ((dbody (denote-term body `((val . (,sym . ,sym)) . senv))))
        (lambda (env) (goal-value (lambda args (dbody (cons args env)))))))))
(define (denote-variable sym senv)
  (let loop ((idx 0) (senv senv))
    (match senv
      (`((val . (,y . ,b)) . ,rest)
        (if (equal? sym y)
          (lambda (env) (goal-value (list-ref env idx)))
          (loop (+ idx 1) rest)))
      (`((rec . ,binding*) . ,rest)
        (let loop-rec ((ridx 0) (binding* binding*))
          (match binding*
            ('() (loop (+ idx 1) rest))
            (`((,p-name ,_) . ,binding*)
              (if (equal? sym p-name)
                (lambda (env)
                  ((list-ref (list-ref env idx) ridx) (drop env idx)))
                (loop-rec (+ ridx 1) binding*))))))
      ('() (error (format "unbound variable: '~a'" sym))))))

(define (denote-qq qqterm senv)
  (match qqterm
    (`(,'unquote ,term) (denote-term term senv))
    (`(,a . ,d) (let ((da (denote-qq a senv)) (dd (denote-qq d senv)))
                  (lambda (env)
                    (let ((ga (da env)) (gd (dd env)))
                      (lambda (st)
                        (let*/state (((st va) (ga st))
                                     ((st va) (actual-value st va #f #f))
                                     ((st vd) (gd st))
                                     ((st vd) (actual-value st vd #f #f)))
                          (values st `(,va . ,vd))))))))
    ((? quotable? datum) (denote-value datum))))
(define (denote-application proc a* senv)
  (denote-apply (denote-term proc senv) (denote-term-list a* senv)))
(define (denote-apply dp da*)
  (lambda (env)
    (let ((gp (dp env)) (ga* (da* env)))
      (lambda (st)
        (let*/state (((st va*) (ga* st)) ((st vp) (gp st)))
          ((apply vp va*) st))))))
(define (denote-term-list terms senv)
  (let ((d* (map (lambda (term) (denote-term term senv)) terms)))
    (lambda (env)
      (let ((g* (map (lambda (d0) (d0 env)) d*)))
        (lambda (st)
          (let loop ((st st) (xs '()) (g* g*))
            (if (null? g*) (values st (reverse xs))
              (let*/state (((st val) ((car g*) st))
                           ((st val) (actual-value st val #f #f)))
                (loop st (cons val xs) (cdr g*))))))))))

(define (denote-rhs-pattern-unknown env st v) st)
(define (denote-rhs-pattern-literal literal)
  (lambda (env st v) (unify st literal v)))
(define (denote-rhs-pattern-var vname senv)
  (let ((dv (denote-term vname senv)))
    (lambda (env st rhs)
      (let*/state (((st v) ((dv env) st))) (unify st v rhs)))))
(define (denote-rhs-pattern-qq qq senv)
  (match qq
    (`(,'unquote ,pat) (denote-rhs-pattern pat senv))
    (`(,a . ,d)
      (let ((da (denote-rhs-pattern-qq a senv))
            (dd (denote-rhs-pattern-qq d senv)))
        (lambda (env st v)
          (let/vars (va vd)
            (let ((v1 `(,va . ,vd)))
              (let*/and ((st (unify st v v1)) (st (da env st va)))
                (dd env st vd)))))))
    ((? quotable? datum) (denote-rhs-pattern-literal datum))))
(define denote-rhs-pattern-true (denote-rhs-pattern-literal #t))
(define denote-rhs-pattern-false (denote-rhs-pattern-literal #f))
(define (denote-rhs-pattern pat senv)
  (match pat
    (`(quote ,(? quotable? datum)) (denote-rhs-pattern-literal datum))
    (`(quasiquote ,qq) (denote-rhs-pattern-qq qq senv))
    ((? symbol? vname) (denote-rhs-pattern-var vname senv))
    ((? number? datum) (denote-rhs-pattern-literal datum))
    (#t denote-rhs-pattern-true)
    (#f denote-rhs-pattern-false)
    (_ denote-rhs-pattern-unknown)))

(define (extract-svs st v1 v2)
  (let-values (((v1 va1) (walk st v1))
               ((v2 va2) (walk st v2)))
    (match* (v1 v2)
      (((? var?) (? var?)) (list v1 v2))
      (((? var?) _) (list v1))
      ((_ (? var?)) (list v2))
      ((`(,a1 . ,d1) `(,a2 . ,d2))
       (list-append-unique (extract-svs st a1 a2) (extract-svs st d1 d2)))
      ((_ _) '()))))

(define p-any '(_))
(define (p-lookup path) `(lookup ,path))
(define (p-literal datum) `(literal ,datum))
(define (p-type tag) `(type ,tag))
(define p-symbol (p-type 'symbol))
(define p-number (p-type 'number))
(define p-pair (p-type 'pair))
(define (p-car p) `(car ,p))
(define (p-cdr p) `(cdr ,p))
(define (p-and p1 p2) `(and ,p1 ,p2))
(define (p-or p1 p2) `(or ,p1 ,p2))
(define (p-not p) `(not ,p))
(define (p-? pred) `(? ,pred))
(define p-none (p-not p-any))

;; TODO: pattern interpretations
;; each analysis task gives an interpretation
;; * assert, usual state update (use only prune for analysis?)
;; * simplify, impossibility-detection
;; * subsume
;; * domain->block and index construction
;; interpretation args: state, penv, parity, value

(define (datum->didx datum)
  (match datum
    (`(,_ . ,_) 0)
    ((? symbol?) 1)
    ((? number?) 2)
    ('() 3)
    (#f 4)
    (#t 5)))
(define (tag->didx tag)
  (match tag
    ('pair 0)
    ('symbol 1)
    ('number 2)
    ('() 3)
    (#f 4)
    (#t 5)))
(define (p->domain parity p)
  (define not-pair (pdomain-complement (pdomain-single (tag->didx 'pair))))
  (define (paritize parity pd) (if parity pd (pdomain-complement pd)))
  (define (paritize-pair parity ppd)
    (if parity ppd (pdomain-join ppd not-pair)))
  (match p
    (`(literal ,datum)
      (match datum
        ((or '() #f #t) (p->domain parity (p-type datum)))
        (`(,dcar . ,dcdr)
          (paritize-pair parity (pdomain-single-pair
                                  (cons (p->domain parity (p-literal dcar))
                                        (p->domain parity (p-literal dcdr))))))
        (_ (if parity (pdomain-single (datum->didx datum)) pdomain-full))))
    (`(type ,tag) (paritize parity (pdomain-single (tag->didx tag))))
    (`(car ,p)
      (paritize-pair parity (pdomain-single-pair
                              (cons (p->domain parity p) pdomain-full))))
    (`(cdr ,p)
      (paritize-pair parity (pdomain-single-pair
                              (cons pdomain-full (p->domain parity p)))))
    (`(and ,p1 ,p2) ((if parity pdomain-meet pdomain-join)
                     (p->domain parity p1) (p->domain parity p2)))
    (`(or ,p1 ,p2) ((if parity pdomain-join pdomain-meet)
                    (p->domain parity p1) (p->domain parity p2)))
    (`(not ,p) (p->domain (not parity) p))
    ('(_) (if parity pdomain-full pdomain-empty))
    (`(lookup ,_) pdomain-full)
    (`(? ,_) pdomain-full)))

(define (ps->index cs st vs vtop)
  (define (extract-pair cs st path v vs)
    (let ((pr (walk1 st v)))
      (ps->index cs st (list* (cons (cons 'car path) (car pr))
                              (cons (cons 'cdr path) (cdr pr)) vs) vtop)))
  (define (extract-literals cs st path v vs)
    ;; TODO: extract, then partition on literals
    ;; Profile to determine if this is necessary (assoc could make this faster)
    (finish cs st path v vs))
  (define (finish cs st path v vs) (and (pair? vs) (ps->index cs st vs vtop)))
  (let ((path (caar vs)) (v (cdar vs)) (v* (cdr vs)))
    (let part ((cs cs)
               (st st)
               (parts '())
               (tag*cont
                 `((pair ,extract-pair)
                   (symbol ,extract-literals)
                   (number ,extract-literals)
                   (() ,finish)
                   (#f ,finish)
                   (#t ,finish))))
      (match (and (< 1 (length cs)) tag*cont)
        (`((,tag ,cont) . ,t*c)
          (let ((st0 (typify st tag v)) (nst0 (distypify st tag v)))
            (if (and st0 nst0)
              (let loop ((c* cs) (sc* '()) (nc* '()))
                ;; TODO: bring in bottom up domain calculation on sc* nc*
                (match c*
                  (`((,p . ,rhs) . ,c*1)
                    (loop
                      c*1
                      (let-values (((st1 p1) (pat-prune p #t st0 vtop vtop)))
                        (if st1 (cons (cons p1 rhs) sc*) sc*))
                      (let-values (((nst1 p1) (pat-prune p #t nst0 vtop vtop)))
                        (if nst1 (cons (cons p1 rhs) nc*) nc*))))
                  (_ (if (or (null? sc*) (null? nc*)
                             (and (= (length cs) (length sc*))
                                  (= (length cs) (length nc*))))
                       (part cs st parts '())
                       (let* ((sc* (reverse sc*))
                              (nc* (reverse nc*))
                              (more (and (< 1 (length sc*))
                                         (cont sc* st0 path v v*))))
                         (part nc* nst0 (cons (list tag sc* more) parts)
                               t*c))))))
              (part cs st v vtop parts t*c))))
        (_ (list path (reverse parts) cs))))))

(define (pdomain pair symbol number nil f t)
  (vector pair symbol number nil f t))
(define (pdomain-pair pd) (vector-ref pd 0))
(define (pdomain-set pd idx v)
  (define pd1 (vector-copy pd))
  (vector-set! pd1 idx v)
  pd1)
(define (pdomain-add pd idx) (pdomain-set pd idx #t))
(define (pdomain-add-pair pd v)
  (pdomain-set
    pd 0 (and (not (or (equal? pdomain-empty (car v))
                       (equal? pdomain-empty (cdr v))))
              (or (and (equal? pdomain-full (car v))
                       (equal? pdomain-full (cdr v)))
                  v))))
(define (pdomain-remove pd idx) (pdomain-set pd idx #f))
(define (pdomain-remove-pair pd) (pdomain-remove pd 0))
(define (pdomain-complement pd)
  (let ((v1 (vector-map not pd)) (pr (pdomain-pair pd)))
    (if (pair? pr)
      (pdomain-add-pair v1 (cons (pdomain-complement (car pr))
                                 (pdomain-complement (cdr pr))))
      v1)))
(define (pdomain-meet pd1 pd2)
  (let ((v1 (vector-map (lambda (a b) (not (not (and a b)))) pd1 pd2))
        (pr1 (pdomain-pair pd1))
        (pr2 (pdomain-pair pd2)))
    (if (pdomain-pair v1)
      (if (pair? pr1)
        (if (pair? pr2)
          (pdomain-add-pair v1 (cons (pdomain-meet (car pr1) (car pr2))
                                     (pdomain-meet (cdr pr1) (cdr pr2))))
          (pdomain-add-pair v1 pr1))
        (if (pair? pr2)
          (pdomain-add-pair v1 pr2)
          v1))
      v1)))
(define (pdomain-join pd1 pd2)
  (let ((v1 (vector-map (lambda (a b) (not (not (or a b)))) pd1 pd2))
        (pr1 (pdomain-pair pd1))
        (pr2 (pdomain-pair pd2)))
    (if (pair? pr1)
      (pdomain-add-pair v1 (if (pair? pr2)
                             (cons (pdomain-join (car pr1) (car pr2))
                                   (pdomain-join (cdr pr1) (cdr pr2)))
                             (if pr2 (cons pdomain-full pdomain-full) pr1)))
      (if (pair? pr2)
        (pdomain-add-pair v1 (if pr1 (cons pdomain-full pdomain-full) pr2))
        v1))))
(define (pdomain-single idx) (pdomain-add pdomain-empty idx))
(define (pdomain-single-pair v) (pdomain-add-pair pdomain-empty v))
(define pdomain-empty (pdomain #f #f #f #f #f #f))
(define pdomain-full (pdomain-complement pdomain-empty))

(define tag*didx
  '((pair . 0)
    (symbol . 1)
    (number . 2)
    (() . 3)
    (#f . 4)
    (#t . 5)))

(define domain*-full (cons domain-full #t))
(define (pdomain->domain pd)
  (let loop ((t*d tag*didx) (dmn domain-full))
    (match t*d
      (`((,tag . ,idx) . ,t*d)
        (loop t*d (if (vector-ref pd idx) dmn
                    (domain-remove dmn tag))))
      (_ dmn))))
(define (pdomain->domain* pd)
  (define dmn (pdomain->domain pd))
  (if (eq? domain-full dmn) domain*-full
    (cons dmn
          (let*/and ((pd-pair (vector-ref pd 0)))
            (or (eq? #t pd-pair)
                (cons (pdomain->domain* (car pd-pair))
                      (pdomain->domain* (cdr pd-pair))))))))

(define (lookup/access access st v)
  (let/vars (va vd)
    (let ((v1 `(,va . ,vd)))
      (let ((st1 (unify st v1 v)))
        (if st1
          (values st1 (walk1 st1 (access v1)))
          (values #f #f))))))
(define (lookup/car st v) (lookup/access car st v))
(define (lookup/cdr st v) (lookup/access cdr st v))
(define (path-lookup path st v)
    (if (null? path) (values st v)
      (let-values (((st1 v1) ((car path) st v)))
        (if st1
          (path-lookup (cdr path) st1 v1)
          (error (format "invalid access path ~v for ~v"
                         path (walk1 st v)))))))

(define (pat-prune p parity st v vtop)
  (define always (if parity p-any p-none))
  (define never (if parity p-none p-any))
  (define (always? p) (eq? always p))
  (define (never? p) (eq? never p))

  (define (prune-cx st cx ncx arg)
    (let/if (st1 ((if parity cx ncx) st arg v))
      (let/if (nst ((if parity ncx cx) st arg v))
        (values st1 p)
        (values st always))
      (values #f #f)))
  (define (prune-pair tag trans p)
    (let/vars (va vd)
      (let ((v1 `(,va . ,vd)))
        (let/if (st1 (unify st v1 v))
          (let-values (((st2 p) (pat-prune p parity st1 (trans v1) vtop)))
            (if st2
              (if (always? p)
                (if parity
                  (pat-prune p-pair #t st v vtop)
                  (values st p))
                (values st2 `(,tag ,p)))
              (values #f #f)))
          (if parity
            (values #f #f)
            (pat-prune p-pair #f st v vtop))))))
  (define (prune-and p1 p2)
    (let-values (((st1 p1) (pat-prune p1 parity st v vtop)))
      (if st1
        (let-values (((st2 p2) (pat-prune p2 parity st1 v vtop)))
          (if st2
            (values
              st2 (if (always? p2) p1
                    (let-values (((st2 _) (pat-prune p2 parity st v vtop)))
                      (let-values (((_ p1) (pat-prune p1 parity st2 v vtop)))
                        (if (always? p1) p2
                          `(,(if parity 'and 'or) ,p1 ,p2))))))
            (values #f #f)))
        (values #f #f))))
  (define (prune-or p1 p2)
    (let-values (((st1 p1-new) (pat-prune p1 parity st v vtop)))
      (if st1
        (let-values (((nst1 _) (pat-prune p1-new (not parity) st v vtop)))
          (if nst1
            (let-values (((st2 p2) (pat-prune p2 parity nst1 v vtop)))
              (if st2
                (if (always? p2) (values st always)
                  (let-values
                    (((nst2 _) (pat-prune p2 (not parity) st v vtop)))
                    (let-values (((st1 p1-new)
                                  (pat-prune p1-new parity nst2 v vtop)))
                      (if st1
                        (if (always? p1-new) (values st always)
                          (values st `(,(if parity 'or 'and) ,p1-new ,p2)))
                        (values st2 p2)))))
                (values st1 p1-new)))
            (values st always)))
        (pat-prune p2 parity st v vtop))))

  (define (lookup->pat path st v1)
    (let ((v1 (walk1 st v1)))
      (and (not (var? v1))
        (if (pair? v1)
          (let* ((pcar (lookup->pat path st (car v1)))
                 (pcdr (lookup->pat path st (cdr v1))))
            (match* (pcar pcdr)
              ((`(literal ,lcar) `(literal ,lcdr)) (p-literal (cons lcar lcdr)))
              ((_ _) #f)))
          (p-literal v1)))))

  (match p
    ('(_) (if parity (values st p) (values #f #f)))
    (`(lookup ,path)
      (let-values (((st1 v1) (path-lookup path st vtop)))
        (let ((pat (lookup->pat path st1 v1)))
          (if pat
            (pat-prune pat parity st1 v vtop)
            (prune-cx st1 unify disunify v1)))))
    (`(literal ,datum) (prune-cx st unify disunify datum))
    (`(type ,tag) (prune-cx st typify distypify tag))
    (`(car ,p) (prune-pair 'car car p))
    (`(cdr ,p) (prune-pair 'cdr cdr p))
    (`(and ,p1 ,p2) (if parity (prune-and p1 p2) (prune-or p1 p2)))
    (`(or ,p1 ,p2) (if parity (prune-or p1 p2) (prune-and p1 p2)))
    (`(not ,p)
      (let-values (((st1 p) (pat-prune p (not parity) st v vtop)))
        (if st1
          (values st1 (if (never? p) always `(not ,p)))
          (values #f #f))))
    (`(? ,_) (values st p))))

(define (match-compile c*)
  (define (didx-push idx pat pd0 pd1)
    (if (vector-ref pd0 idx)
      (let ((p*1 (vector-ref pd1 idx)))
        (pdomain-set pd1 idx (if p*1 (cons pat p*1) (list pat))))
      pd1))
  (define (pdomain-push pd0 pd1 pat)
    ;; TODO: treat pair push specially, for sub-indexing
    (didx-push 5 pat pd0
      (didx-push 4 pat pd0
        (didx-push 3 pat pd0
          (didx-push 2 pat pd0
            (didx-push 1 pat pd0
              (didx-push 0 pat pd0 pd1)))))))
  (let loop ((c* c*) (p-context p-any))
    (match c*
      (`((,pat . (,rhs . ,rhspat)) . ,c*1)
        (let-values (((st pat1)
                      (pat-prune
                        (p-and pat p-context) #t state-empty var-0 var-0)))
          (if st
            (let-values (((c*1 dmn pd1)
                          (loop c*1 (p-and (p-not pat) p-context))))
              ;; TODO: define the rest of this recursively, to allow pair indexing
              (let* ((pd0 (p->domain #t pat1))
                     (dmn-narrow (pdomain->domain pd0))
                     (st-dmn-narrow (domainify state-empty var-0 dmn-narrow)))
                ;; pushed versions of patterns use narrow domain for even more subsumption
                ;;   safe due to known-type dispatch
                (let-values (((_ pat1-narrow)
                              (pat-prune pat1 #t st-dmn-narrow var-0 var-0)))
                  (let* ((clause-narrow `(,pat1-narrow . (,rhs . ,rhspat)))
                         (pd1 (pdomain-push pd0 pd1 clause-narrow))
                         (dmn-new (pdomain->domain pd1))
                         (dmn-new (if (equal? dmn-new dmn) dmn dmn-new))
                         (st-dmn (domainify state-empty var-0 dmn-new)))
                    ;; use tail-inclusive domain for more pattern subsumption
                    (let-values (((_ pat1)
                                  (pat-prune pat1 #t st-dmn var-0 var-0)))
                      (let ((clause `(,pat1 . (,rhs . ,rhspat))))
                        (values `(,dmn-new ,clause . ,c*1) dmn-new pd1)))))))
            (loop c*1 p-context))))
      (_ (values '() '() pdomain-empty))))) ;; TODO: rhs index if not intermediate

(define (pattern-assert-any parity st penv v)
  (if parity
    (values st penv '())
    (values #f #f #f)))
(define (pattern-assert-none parity st penv v)
  (if parity
    (values #f #f #f)
    (values st penv '())))

(define (pattern-value-literal literal) (lambda (st penv) literal))
(define (pattern-value-ref index)
  (lambda (st penv) (walk1 st (list-ref penv index))))
(define (pattern-var-extend parity st penv v)
  (if parity
    (values st (cons (walk1 st v) penv) '())
    (values #f #f #f)))

(define (pattern-transform f assert)
  (lambda (parity st penv v) (assert parity st penv (f v))))
(define (pattern-replace v assert) (pattern-transform (lambda (_) v) assert))

(define (pattern-assert-not assert)
  (lambda (parity st penv v)
    (let-values (((st _ svs) (assert (not parity) st penv v)))
      (values st penv svs))))
(define (pattern-assert-== pvalue)
  (lambda (parity st penv v)
    (let ((pval (pvalue st penv)))
      (let/if (st1 ((if parity unify disunify) st pval v))
        (values st1 penv (extract-svs st pval v))
        (values #f #f #f)))))
(define (pattern-assert-=/= pvalue)
  (pattern-assert-not (pattern-assert-== pvalue)))
(define (pattern-assert-type-== type-tag)
  (lambda (parity st penv v)
    (let/if (st1 ((if parity typify distypify) st type-tag v))
      (let ((v1 (walk1 st v)))
        (values st1 penv (if (var? v1) (list v1) '())))
      (values #f #f #f))))

(define pattern-assert-pair-== (pattern-assert-type-== 'pair))
(define pattern-assert-symbol-== (pattern-assert-type-== 'symbol))
(define pattern-assert-number-== (pattern-assert-type-== 'number))
(define (pattern-assert-pair assert-car assert-cdr)
  (define assert (pattern-assert-and (pattern-transform car assert-car)
                                     (pattern-transform cdr assert-cdr)))
  (lambda (parity st penv v)
    (let ((v (walk1 st v)))
      ((cond
         ((pair? v) assert)
         ((var? v)
          (let/vars (va vd)
            (let ((v1 `(,va . ,vd)))
              (lambda (parity st penv v)
                (let-values
                  (((st penv svs)
                    ((pattern-assert-and
                       (lambda (parity st penv v)
                         ((if parity
                            (pattern-assert-== (pattern-value-literal v1))
                            pattern-assert-pair-==)
                          parity st penv v))
                       (pattern-replace v1 assert))
                     parity st penv v)))
                  (values st penv (and svs (list-subtract
                                             svs (list va vd)))))))))
         (else pattern-assert-none))
       parity st penv v))))

(define pattern-assert-false
  (pattern-assert-== (pattern-value-literal #f)))
(define pattern-assert-not-false
  (pattern-assert-=/= (pattern-value-literal #f)))
(define (pattern-assert-predicate pred)
  (lambda (parity st penv v)
    (let-values (((st result) (pred st v)))
      ; TODO: if (not st), the entire computation needs to fail, not just this
      ; particular pattern assertion.  This failure needs to cooperate with
      ; nondeterministic search, which makes things more complex and likely
      ; less efficient (negations of conjunctions containing predicates become
      ; mandatory, to verify that the predicate invocation, if reached, returns
      ; a value).  For now, we'll assume a predicate can never fail in this
      ; manner.  In this implementation, such a failure leads to unsound
      ; behavior by being unpredictable in the granularity of failure.
      (if (match-chain? result)
        (let-values (((st svs result) (match-chain-try st result #f #f)))
          (if (match-chain? result)
            (let-values (((st rhs) (if parity
                                     (let/vars (notf)
                                       (values (disunify st notf #f) notf))
                                     (values st #f))))
              (let-values (((st result) (actual-value st result #t rhs)))
                (if st
                  (values st penv svs)
                  (values #f #f #f))))
            (pattern-assert-not-false parity st penv result)))
        (pattern-assert-not-false parity st penv result)))))

(define (pattern-exec-and a1 a2 st penv v)
  (let-values (((st penv svs1) (a1 #t st penv v)))
    (if st
      (let-values (((st penv svs2) (a2 #t st penv v)))
        (if st
          (values st penv (list-append-unique svs1 svs2))
          (values #f #f #f)))
      (values #f #f #f))))
(define (pattern-assert-and a1 a2)
  (define (nassert)
    (pattern-assert-or (pattern-assert-not a1)
                       (pattern-assert-and a1 (pattern-assert-not a2))))
  (lambda (parity st penv v)
    (if parity
      (pattern-exec-and a1 a2 st penv v)
      ((nassert) #t st penv v))))

(define or-rhs (cons denote-true denote-rhs-pattern-unknown))
(define (pattern-assert-or a1 a2)
  (define na1 (pattern-assert-not a1))
  (define na2 (pattern-assert-not a2))
  (define clause* (list (cons (lambda (env) a1) or-rhs)
                        (cons (lambda (env) a2) or-rhs)))
  (lambda (parity st penv v)
    (if parity
      (let-values (((st svs result)
                    (match-chain-try st (mc-new penv '() v clause* #f) #f #t)))
        (if (match-chain? result)
          (values (match-chain-suspend st #f result svs #t) penv svs)
          (values st penv svs)))
      (pattern-exec-and na1 na2 st penv v))))

(define (denote-pattern-succeed env) pattern-assert-any)
(define (denote-pattern-fail env) pattern-assert-none)
(define (denote-pattern-literal literal penv)
  (values penv (let ((assert (pattern-assert-==
                               (pattern-value-literal literal))))
                 (lambda (env) assert))))
(define (denote-pattern-var-extend env) pattern-var-extend)
(define (denote-pattern-var b* vname penv)
  (let loop ((idx 0) (b* b*))
    (match b*
      ('() (values `((,vname ,vname) . ,penv) denote-pattern-var-extend))
      (`((,name ,x) . ,b*)
        (if (eq? name vname)
          (values penv (let ((assert (pattern-assert-==
                                       (pattern-value-ref idx))))
                         (lambda (env) assert)))
          (loop (+ idx 1) b*))))))
(define (denote-pattern-qq qqpattern penv senv)
  (match qqpattern
    (`(,'unquote ,pat) (denote-pattern pat penv senv))
    (`(,a . ,d)
      (let*-values (((penv da) (denote-pattern-qq a penv senv))
                    ((penv dd) (denote-pattern-qq d penv senv)))
        (values penv (lambda (env) (pattern-assert-pair (da env) (dd env))))))
    ((? quotable? datum) (denote-pattern-literal datum penv))))
(define (denote-pattern-cons apat dpat penv senv)
  (let*-values (((penv da) (denote-pattern apat penv senv))
                ((penv dd) (denote-pattern dpat penv senv)))
    (values penv (lambda (env) (pattern-assert-pair (da env) (dd env))))))

(define (denote-pattern-or pattern* penv senv)
  (match pattern*
    ('() (values penv denote-pattern-fail))
    (`(,pat) (denote-pattern pat penv senv))
    (`(,pat . ,pat*)
      (let*-values (((_ d0) (denote-pattern pat penv senv))
                    ((_ d*) (denote-pattern-or pat* penv senv)))
        (values penv (lambda (env) (pattern-assert-or (d0 env) (d* env))))))))
(define (denote-pattern* pattern* penv senv)
  (match pattern*
    ('() (values penv denote-pattern-succeed))
    (`(,pat) (denote-pattern pat penv senv))
    (`(,pat . ,pat*)
      (let*-values (((penv d0) (denote-pattern pat penv senv))
                    ((penv d*) (denote-pattern* pat* penv senv)))
        (values penv (lambda (env) (pattern-assert-and (d0 env) (d* env))))))))
(define (denote-pattern-not pat* penv senv)
  (let-values (((_ dp) (denote-pattern* pat* penv senv)))
    (values penv (lambda (env) (pattern-assert-not (dp env))))))

(define (denote-pattern-symbol env) pattern-assert-symbol-==)
(define (denote-pattern-number env) pattern-assert-number-==)
(define (denote-pattern-type type-tag pat* penv senv)
  (define dty (match type-tag
                ('symbol denote-pattern-symbol)
                ('number denote-pattern-number)))
  (if (null? pat*) (values penv dty)
    (let-values (((penv d*) (denote-pattern* pat* penv senv)))
      (values penv (lambda (env) (pattern-assert-and (dty env) (d* env)))))))
(define (denote-pattern-? predicate pat* penv senv)
  (let ((dpred (denote-term predicate senv)))
    (let-values (((penv d*) (denote-pattern* pat* penv senv)))
      (values penv
              (lambda (env)
                (let-values (((st0 vpred) ((dpred env) #t)))
                  (if (not st0)
                    (error (format "invalid predicate: ~a" predicate))
                    (let* ((pred (lambda (st v)
                                   (let-values (((st result) ((vpred v) st)))
                                     (if (not st)
                                       (error (format "predicate failed: ~a"
                                                      predicate))
                                       (values st result)))))
                           (assert (pattern-assert-predicate pred)))
                      (if (null? pat*) assert
                        (pattern-assert-and assert (d* env)))))))))))

(define (denote-pattern pat penv senv)
  (match pat
    (`(quote ,(? quotable? datum)) (denote-pattern-literal datum penv))
    (`(quasiquote ,qqpat) (denote-pattern-qq qqpat penv senv))
    (`(cons ,apat ,dpat) (denote-pattern-cons apat dpat penv senv))
    (`(not . ,pat*) (denote-pattern-not pat* penv senv))
    (`(and . ,pat*) (denote-pattern* pat* penv senv))
    (`(or . ,pat*) (denote-pattern-or pat* penv senv))
    (`(symbol . ,pat*) (denote-pattern-type 'symbol pat* penv senv))
    (`(number . ,pat*) (denote-pattern-type 'number pat* penv senv))
    (`(? ,predicate . ,pat*) (denote-pattern-? predicate pat* penv senv))
    ('_ (values penv denote-pattern-succeed))
    ((? symbol? vname) (denote-pattern-var penv vname penv))
    ((? quotable? datum) (denote-pattern-literal datum penv))))

(defrec match-chain scrutinee penv env clauses active?)
(define (mc-new penv0 env scrutinee clauses active?)
  (match-chain scrutinee penv0 env clauses active?))
(define mc-scrutinee match-chain-scrutinee)
(define mc-penv match-chain-penv)
(define mc-env match-chain-env)
(define mc-clauses match-chain-clauses)
(define mc-active? match-chain-active?)

(define (actual-value st result rhs? rhs)
  (if (match-chain? result)
    (let-values (((st svs result) (match-chain-try st result rhs? rhs)))
      (if (match-chain? result)
        (let ((rhs (if rhs? rhs (let/vars (rhs) rhs))))
          (values (match-chain-suspend st #f result svs rhs) rhs))
        (values st result)))
    (values (if rhs? (and st (unify st result rhs)) st) result)))

(define (match-chain-suspend st goal-ref mc svs rhs)
  (let* ((rhs (walk1 st rhs))
         (goal-ref (or goal-ref (goal-ref-new)))
         (retry (lambda (st)
                  (let ((rhs (walk1 st rhs)))
                    (let-values (((st svs result)
                                (match-chain-try st mc #t rhs)))
                      (if (match-chain? result)
                        (match-chain-suspend st goal-ref result svs rhs)
                        (and st (state-remove-goal st goal-ref)))))))
         (guess (lambda (st)
                  (match-chain-guess goal-ref st mc (walk1 st rhs))))
         (goal (goal-suspended
                 #f rhs svs retry guess (mc-active? mc))))
    (state-suspend* st svs (value->vars st rhs) goal-ref goal)))

(define (rhs->goal rhs? rhs)
  (lambda (svs penv env st drhs)
    (let-values (((st result) ((drhs (append penv env)) st)))
      (if (match-chain? result)
        (match-chain-try st result rhs? rhs)
        (values (if rhs? (and st (unify st result rhs)) st) svs result)))))

(define (match-chain-guess goal-ref st mc rhs)
  (define v (walk1 st (mc-scrutinee mc)))
  (define penv0 (mc-penv mc))
  (define env (mc-env mc))
  (define pc* (mc-clauses mc))
  (define active? (mc-active? mc))
  (define run-rhs (rhs->goal #t rhs))
  (define (commit-without next-pc* assert)
    (let-values (((st penv _) (assert #f st penv0 v)))
      (and st
           (let-values (((st svs result)
                         (match-chain-try
                           st (mc-new penv0 env v next-pc* active?) #t rhs)))
             (if (match-chain? result)
               (match-chain-suspend st goal-ref result svs rhs)
               (and st (state-remove-goal st goal-ref)))))))
  (define (commit-with assert drhs)
    (let-values (((st penv svs)
                  (assert #t (state-remove-goal st goal-ref) penv0 v)))
      (and st (let-values (((st svs result) (run-rhs svs penv env st drhs)))
                (if (match-chain? result)
                  (match-chain-suspend st #f result svs rhs)
                  st)))))
  (and (pair? pc*)
       (zzz (let* ((next-pc* (cdr pc*))
                   (assert ((caar pc*) env))
                   (drhs (cadar pc*))
                   (ss (reset-cost (commit-with assert drhs))))
              (if (pair? next-pc*)
                  (mplus ss (zzz (reset-cost (commit-without next-pc* assert))))
                  ss)))))

(define (match-chain-try st mc rhs? rhs)
  (let* ((rhs (if rhs? (walk1 st rhs) rhs))
         (run-rhs (rhs->goal rhs? rhs))
         (v (mc-scrutinee mc))
         (penv0 (mc-penv mc))
         (env (mc-env mc))
         (pc* (mc-clauses mc))
         (active? (mc-active? mc)))
    (let ((v (walk1 st v)))
      (let loop ((st st) (pc* pc*))
        (if (null? pc*) (values #f #f #f)
          (let* ((assert ((caar pc*) env))
                 (drhs (cadar pc*))
                 (drhspat (cddar pc*)))
            (let-values (((st1 penv1 svs) (assert #t st penv0 v)))
              (let ((commit (lambda () (run-rhs svs penv1 env st1 drhs))))
                ;; Is the first pattern satisfiable?
                (if st1
                  ;; If we only have a single option, commit to it.
                  (if (null? (cdr pc*)) (commit)
                    (begin (det-pay 1)
                      ;; If no vars were scrutinized (svs) while checking
                      ;; satisfiability, then we have an irrefutable match, so
                      ;; commit to it.
                      (if (null? svs) (commit)
                        ;; If vars were scrutinized, there is room for doubt.
                        ;; Check whether the negated pattern is satisfiable.
                        (begin (det-pay 5)
                          (let-values (((nst _ nsvs) (assert #f st penv0 v)))
                            (if nst
                              ;; If the negation is also satisfiable, check whether
                              ;; we can still rule out this clause by matching its
                              ;; right-hand-side with the expected result of the
                              ;; entire match expression.
                              (if (and rhs? (not (drhspat
                                                   (append penv1 env) st1 rhs)))
                                ;; If we can rule it out, permanently learn the
                                ;; negated state.
                                (loop nst (cdr pc*))
                                ;; Otherwise, we're not sure whether to commit to
                                ;; this clause yet.  If there are no other
                                ;; satisfiable patterns, we can.  If there is at
                                ;; least one other satisfiable pattern, we should
                                ;; wait until later, when we either have more
                                ;; information, or we're forced to guess.
                                (let ambiguous ((nst nst) (pc*1 (cdr pc*)) (nc* '()))
                                  (let ((assert1 ((caar pc*1) env))
                                        (drhspat1 (cddar pc*1)))
                                    ;; Is the next pattern satisfiable?
                                    (let-values (((st1 penv1 svs1)
                                                  (assert1 #t nst penv0 v)))
                                      (if st1
                                        ;; If it is, check whether we can rule it out
                                        ;; by matching its right-hand-side with the
                                        ;; expected result.
                                        (if (and rhs?
                                                 (not (drhspat1
                                                        (append penv1 env) st1 rhs)))
                                          ;; If we rule it out and there are no
                                          ;; patterns left to try, the first clause
                                          ;; is the only option.  Commit to it.
                                          (if (null? (cdr pc*1)) (commit)
                                            ;; If we rule it out and there are other
                                            ;; patterns to try, learn the negation
                                            ;; and continue the search.
                                            (let-values (((nst1 _ __)
                                                          (assert1 #f nst penv0 v)))
                                              (if nst1
                                                ;; Clauses that were ruled out (nc*)
                                                ;; need to be tracked so that retries
                                                ;; can relearn their negated
                                                ;; patterns.
                                                (ambiguous
                                                  nst1 (cdr pc*1) (cons (car pc*1)
                                                                        nc*))
                                                ;; Unless the negation is impossible,
                                                ;; in which case nothing else could
                                                ;; succeed, meaning the first clause
                                                ;; is the only option after all!
                                                ;; Commit to it.
                                                (commit))))
                                          ;; If we can't rule it out, then we've
                                          ;; established ambiguity.  Try again later.
                                          (values st
                                                  (list-append-unique
                                                    svs1 (list-append-unique
                                                           nsvs svs))
                                                  (mc-new
                                                    penv0 env v (cons (car pc*)
                                                                      (rev-append
                                                                        nc* pc*1))
                                                    active?)))
                                        ;; Otherwise, if we have no other clauses
                                        ;; available, then the first clause happens
                                        ;; to be the only option.  Commit to it.
                                        (if (null? (cdr pc*1)) (commit)
                                          ;; If the there still are other clauses,
                                          ;; keep checking.
                                          (ambiguous nst (cdr pc*1) nc*)))))))
                              ;; If the negated pattern wasn't satisfiable, this
                              ;; pattern was irrefutable after all.  Commit.
                              (commit)))))))
                      ;; If the first pattern wasn't satisfiable, throw that clause
                      ;; away and try the next.
                      (loop st (cdr pc*)))))))))))

(define (pattern-match penv dv pc* active?)
  (lambda (env)
    (let ((gv (dv env)))
      (lambda (st)
        (let*/state (((st v) (gv st))
                     ((st v) (actual-value st v #f #f)))
          (values st (mc-new penv env v pc* active?)))))))

(define (denote-match pt*-all vt senv active?)
  (let ((dv (denote-term vt senv))
        (pc* (let loop ((pt* pt*-all))
               (match pt*
                 ('() '())
                 (`((,pat ,rhs) . ,clause*)
                   (let*-values
                     (((penv dpat) (denote-pattern pat '() senv))
                      ((ps vs) (split-bindings penv)))
                     (let* ((senv (extend-env* (reverse ps) (reverse vs) senv))
                            (drhs (denote-term rhs senv))
                            (drhspat (denote-rhs-pattern rhs senv))
                            (pc* (loop clause*)))
                       (cons (cons dpat (cons drhs drhspat)) pc*))))))))
    (pattern-match '() dv pc* active?)))

(define and-rhs (cons denote-false denote-rhs-pattern-false))
(define (denote-and t* senv)
  (match t*
    ('() (denote-value #t))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let* ((d0 (denote-term t senv))
             (d* (denote-and t* senv))
             (clause* (list (cons (lambda (env) pattern-assert-false) and-rhs)
                            (cons (lambda (env) pattern-assert-any)
                                  (cons d* denote-rhs-pattern-unknown)))))
        (lambda (env)
          (let ((g0 (d0 env)))
            (lambda (st)
              (let*/state (((st v0) (g0 st))
                           ((st v0) (actual-value st v0 #f #f)))
                (values st (mc-new '() env v0 clause* #t))))))))))
(define or-rhs-var (gensym 'or-rhs-var))
(define or-rhs-params (list or-rhs-var))
(define (denote-or t* senv)
  (match t*
    ('() (denote-value #f))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let* ((d0 (denote-term t senv))
             (d* (denote-or t* senv))
             (senv1 (extend-env* or-rhs-params or-rhs-params senv))
             (or-2 (denote-term or-rhs-var senv1))
             (or-2-pat (denote-rhs-pattern or-rhs-var senv1))
             (clause* (list (cons (lambda (env) pattern-assert-false)
                                  (cons d* denote-rhs-pattern-unknown))
                            (cons (lambda (env) pattern-var-extend)
                                  (cons or-2 or-2-pat)))))
        (lambda (env)
          (let ((g0 (d0 env)))
            (lambda (st)
              (let*/state (((st v0) (g0 st))
                           ((st v0) (actual-value st v0 #f #f)))
                (values st (mc-new '() env v0 clause* #t))))))))))

(define (denote-fresh vsyms body senv)
  (let ((db (denote-term body (extend-env* vsyms vsyms senv))))
    (lambda (env)
      (let ((vs (map (lambda (vsym) (var (gensym (symbol->string vsym))))
                     vsyms)))
        (db (rev-append vs env))))))

(define (denote-term term senv)
  (let ((bound? (lambda (sym) (in-env? senv sym))))
    (match term
      (#t (denote-value #t))
      (#f (denote-value #f))
      ((? number? num) (denote-value num))
      ((? symbol? sym) (denote-variable sym senv))
      ((and `(,op . ,_) operation)
       (match operation
         (`(,(or (? bound?) (not (? symbol?))) . ,rands)
           (denote-application op rands senv))
         (`(quote ,(? quotable? datum)) (denote-value datum))
         (`(quasiquote ,qqterm) (denote-qq qqterm senv))
         (`(if ,condition ,alt-true ,alt-false)
           (denote-match `((#f ,alt-false) (_ ,alt-true)) condition senv #t))
         (`(lambda ,params ,body) (denote-lambda params body senv))
         (`(let ,binding* ,let-body)
           (let-values (((ps vs) (split-bindings binding*)))
             (denote-apply (denote-lambda ps let-body senv)
                           (denote-term-list vs senv))))
         (`(letrec ,binding* ,letrec-body)
           (let* ((rsenv `((rec . ,binding*) . ,senv))
                  (dbody (denote-term letrec-body rsenv))
                  (db* (let loop ((binding* binding*))
                         (match binding*
                           ('() '())
                           (`((,_ (lambda ,params ,body)) . ,b*)
                             (cons (denote-lambda params body rsenv)
                                   (loop b*)))))))
             (lambda (env) (dbody (cons db* env)))))
         (`(and . ,t*) (denote-and t* senv))
         (`(or . ,t*) (denote-or t* senv))
         (`(match ,scrutinee . ,pt*) (denote-match pt* scrutinee senv #t))
         (`(match/lazy ,scrutinee . ,pt*) (denote-match pt* scrutinee senv #f))
         (`(fresh ,vars ,body) (denote-fresh vars body senv)))))))

(define (in-env? env sym)
  (match env
    ('() #f)
    (`((val . (,a . ,_)) . ,d) (or (equal? a sym) (in-env? d sym)))
    (`((rec . ,binding*) . ,d) (in-env-rec? binding* d sym))))
(define (in-env-rec? binding* env sym)
  (match binding*
    ('() (in-env? env sym))
    (`((,a . ,_) . ,d) (or (equal? a sym) (in-env-rec? d env sym)))))
(define (extend-env* params args env)
  (match `(,params . ,args)
    (`(() . ()) env)
    (`((,x . ,dx*) . (,a . ,da*))
      (extend-env* dx* da* `((val . (,x . ,a)) . ,env)))))

(define (not-in-params? ps sym)
  (match ps
    ('() #t)
    (`(,a . ,d)
      (and (not (equal? a sym)) (not-in-params? d sym)))))
(define (param-list? x)
  (match x
    ('() #t)
    (`(,(? symbol? a) . ,d)
      (and (param-list? d) (not-in-params? d a)))
    (_ #f)))
(define (params? x)
  (match x
    ((? param-list?) #t)
    (x (symbol? x))))
(define (bindings? b*)
  (match b*
    ('() #t)
    (`((,p ,v) . ,b*) (bindings? b*))
    (_ #f)))
(define (split-bindings b*)
  (match b*
    ('() (values '() '()))
    (`((,param ,val) . ,b*)
      (let-values (((ps vs) (split-bindings b*)))
        (values (cons param ps) (cons val vs))))))

(define (match-clauses? pt* env)
  (define (pattern-var? b* vname ps)
    (match b*
      ('() `(,vname . ,ps))
      (`(,name . ,b*) (if (eq? name vname) ps (pattern-var? b* vname ps)))))
  (define (pattern-qq? qqpattern ps env)
    (match qqpattern
      (`(,'unquote ,pat) (pattern? pat ps env))
      (`(,a . ,d) (let*/and ((ps (pattern-qq? a ps env)))
                    (pattern-qq? d ps env)))
      ((? quotable?) ps)))
  (define (pattern-or? pattern* ps env)
    (match pattern*
      ('() ps)
      (`(,pattern . ,pattern*)
        (and (pattern? pattern ps env) (pattern-or? pattern* ps env)))))
  (define (pattern*? pattern* ps env)
    (match pattern*
      ('() ps)
      (`(,pattern . ,pattern*)
        (let*/and ((ps (pattern? pattern ps env)))
          (pattern*? pattern* ps env)))))
  (define (pattern? pattern ps env)
    (match pattern
      (`(quote ,(? quotable?)) ps)
      (`(quasiquote ,qqpat) (pattern-qq? qqpat ps env))
      (`(not . ,pat*) (and (pattern*? pat* ps env) ps))
      (`(and . ,pat*) (pattern*? pat* ps env))
      (`(or . ,pat*) (pattern-or? pat* ps env))
      (`(symbol . ,pat*) (pattern*? pat* ps env))
      (`(number . ,pat*) (pattern*? pat* ps env))
      (`(? ,predicate . ,pat*)
        (and (term? predicate env) (pattern*? pat* ps env)))
      ('_ ps)
      ((? symbol? vname) (pattern-var? ps vname ps))
      ((? quotable?) ps)))
  (match pt*
    ('() #t)
    (`((,pat ,rhs) . ,pt*)
      (let*/and ((ps (pattern? pat '() env)))
        (term? rhs (extend-env* ps ps env)) (match-clauses? pt* env)))))

(define (term-qq? qqterm env)
  (match qqterm
    (`(,'unquote ,term) (term? term env))
    (`(,a . ,d) (and (term-qq? a env) (term-qq? d env)))
    (datum (quotable? datum))))
(define (term? term env)
  (letrec ((term1? (lambda (v) (term? v env)))
           (terms? (lambda (ts env)
                     (match ts
                       ('() #t)
                       (`(,t . ,ts) (and (term? t env) (terms? ts env))))))
           (binding-lambdas?
             (lambda (binding* env)
               (match binding*
                 ('() #t)
                 (`((,_ ,(and `(lambda ,_ ,_) t)) . ,b*)
                   (and (term? t env) (binding-lambdas? b* env)))))))
    (match term
      (#t #t)
      (#f #t)
      ((? number?) #t)
      ((and (? symbol? sym)) (in-env? env sym))
      (`(,(? term1?) . ,rands) (terms? rands env))
      (`(quote ,datum) (quotable? datum))
      (`(quasiquote ,qqterm) (term-qq? qqterm env))
      (`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
      (`(lambda ,params ,body)
        (and (params? params)
             (let ((res (match params
                          ((not (? symbol? params))
                           (extend-env* params params env))
                          (sym `((val . (,sym . ,sym)) . ,env)))))
               (term? body res))))
      (`(let ,binding* ,let-body)
        (and (bindings? binding*)
             (let-values (((ps vs) (split-bindings binding*)))
               (and (terms? vs env)
                    (term? let-body (extend-env* ps ps env))))))
      (`(letrec ,binding* ,letrec-body)
        (let ((res `((rec . ,binding*) . ,env)))
          (and (binding-lambdas? binding* res) (term? letrec-body res))))
      (`(and . ,t*) (terms? t* env))
      (`(or . ,t*) (terms? t* env))
      (`(match ,s . ,pt*) (and (term1? s) (match-clauses? pt* env)))
      (`(fresh ,vars ,body) (term? body (extend-env* vars vars env)))
      (_ #f))))

(define (eval-denoted-term dterm env) (dterm (map cddr env)))
(define (eval-term term env) (eval-denoted-term (denote-term term env) env))

;; 'dk-term' must be a valid dKanren program, *not* just any miniKanren term.
;; 'result' is a miniKanren term.
(define (dk-evalo dk-term expected)
  (let ((dk-goal (eval-term dk-term initial-env)))
    (lambda (st)
      (reset-cost
        (let-values (((st result) (dk-goal st)))
          (and st (let-values (((st _) (actual-value st result #t expected)))
                    st)))))))

(define (primitive params body)
  (let-values (((st v) (((denote-lambda params body '()) '()) #t)))
    (if (not st)
      (error (format "invalid primitive: ~a" `(lambda ,params ,body)))
      v)))

(define empty-env '())
(define initial-env `((val . (cons . ,(primitive '(a d) '`(,a . ,d))))
                      (val . (car . ,(primitive '(x) '(match x
                                                        (`(,a . ,d) a)))))
                      (val . (cdr . ,(primitive '(x) '(match x
                                                        (`(,a . ,d) d)))))
                      (val . (null? . ,(primitive '(x) '(match x
                                                          ('() #t)
                                                          (_ #f)))))
                      (val . (pair? . ,(primitive '(x) '(match x
                                                          (`(,a . ,d) #t)
                                                          (_ #f)))))
                      (val . (symbol? . ,(primitive '(x) '(match x
                                                            ((symbol) #t)
                                                            (_ #f)))))
                      (val . (number? . ,(primitive '(x) '(match x
                                                            ((number) #t)
                                                            (_ #f)))))
                      (val . (not . ,(primitive '(x) '(match x
                                                        (#f #t)
                                                        (_ #f)))))
                      (val . (equal? . ,(primitive '(x y) '(match `(,x . ,y)
                                                             (`(,a . ,a) #t)
                                                             (_ #f)))))
                      (val . (list . ,(primitive 'x 'x)))
                      . ,empty-env))

(module+ test
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

  (test "basic-0"
    (run* (x y))
    '((_.0 _.1)))
  (test "basic-1"
    (run* (q) (== q 3))
    '((3)))
  (test "basic-2"
    (run* (q) (== q 4) (== q 4))
    '((4)))
  (test "basic-3"
    (run* (q) (== q 4) (== q 5))
    '())
  (test "basic-4"
    (run* (q) (== '(1 2) q))
    '(((1 2))))

  (test "basic-5"
    (run* (q) (not-numbero q) (== q 3))
    '())
  (test "basic-6"
    (run* (q) (== q 3) (not-numbero q))
    '())
  (test "basic-7"
    (run* (q) (not-numbero q) (== q 'ok))
    '((ok)))
  (test "basic-8"
    (run* (q) (== q 'ok) (not-numbero q))
    '((ok)))
  (test "basic-9"
    (run* (q) (not-symbolo q) (== q 'ok))
    '())
  (test "basic-10"
    (run* (q) (== q 'ok) (not-symbolo q))
    '())
  (test "basic-11"
    (run* (q) (not-symbolo q) (== q 3))
    '((3)))
  (test "basic-12"
    (run* (q) (== q 3) (not-symbolo q))
    '((3)))
  (test "basic-13"
    (run* (q) (not-pairo q) (== '(1 2) q))
    '())
  (test "basic-14"
    (run* (q) (== '(1 2) q) (not-pairo q))
    '())
  (test "basic-15"
    (run* (q) (not-numbero q) (== '(1 2) q))
    '(((1 2))))
  (test "basic-16"
    (run* (q) (== '(1 2) q) (not-numbero q))
    '(((1 2))))
  (test "basic-17"
    (run* (q) (=/= #f q) (== #f q))
    '())
  (test "basic-18"
    (run* (q) (== #f q) (=/= #f q))
    '())
  (test "basic-19"
    (run* (q) (=/= #t q) (== #f q))
    '((#f)))
  (test "basic-20"
    (run* (q) (== #f q) (=/= #t q))
    '((#f)))

  (test "closed-world-1"
    (run* (q) (=/= '() q) (=/= #f q) (not-pairo q) (not-numbero q) (not-symbolo q))
    '((#t)))
  (test "closed-world-2"
    (run* (q) (=/= '() q) (=/= #t q) (=/= #f q) (not-numbero q) (not-symbolo q))
    '(((_.0 . _.1))))

  (test "absento-ground-1"
    (run* (q) (== q 'ok) (absento 5 '(4 ((3 2) 4))))
    '((ok)))
  (test "absento-ground-2"
    (run* (q) (== q 'ok) (absento 5 '(4 ((3 5) 4))))
    '())
  (test "absento-var-1"
    (run* (q) (== q 'ok) (fresh (r) (== r '(4 ((3 2) 4))) (absento 5 r)))
    '((ok)))
  (test "absento-var-2"
    (run* (q) (== q 'ok) (fresh (r) (== r '(4 ((3 5) 4))) (absento 5 r)))
    '())
  (test "absento-partial-1"
    (run* (q) (== q 2) (absento 5 `(4 ((3 ,q) 4))))
    '((2)))
  (test "absento-partial-2"
    (run* (q) (== q 5) (absento 5 `(4 ((3 ,q) 4))))
    '())
  (test "absento-partial-nested-1"
    (run* (q r) (== q `(1 ,r)) (== r 2) (absento 5 `(4 ((3 ,q) 4))))
    '(((1 2) 2)))
  (test "absento-partial-nested-2"
    (run* (q r) (== q `(1 ,r)) (== r 5) (absento 5 `(4 ((3 ,q) 4))))
    '())
  (test "absento-delayed-var-1"
    (run* (q) (== q 'ok) (fresh (r) (absento 5 r) (== r '(4 ((3 2) 4)))))
    '((ok)))
  (test "absento-delayed-var-2"
    (run* (q) (== q 'ok) (fresh (r) (absento 5 r) (== r '(4 ((3 5) 4)))))
    '())
  (test "absento-delayed-partial-1"
    (run* (q) (absento 5 `(4 ((3 ,q) 4))) (== q 2))
    '((2)))
  (test "absento-delayed-partial-2"
    (run* (q) (absento 5 `(4 ((3 ,q) 4))) (== q 5))
    '())
  ;(test "absento-unknown-1"
    ;(run* (q) (absento 5 q))
    ;'((ok)))
  ;(test "absento-unknown-2"
    ;(run* (q) (absento 5 `(4 ((3 ,q) 4))))
    ;'((ok)))
  ;(test "absento-=/=-1"
    ;(run* (q) (=/= q 5) (absento 5 q))
    ;'((ok)))
  ;(test "absento-=/=-2"
    ;(run* (q) (=/= q 5) (absento 5 `(4 ((3 ,q) 4))))
    ;'((ok)))

  (test "=/=-1"
    (run* (q)
      (=/= 3 q)
      (== q 3))
    '())
  (test "=/=-2"
    (run* (q)
      (== q 3)
      (=/= 3 q))
    '())
  (test "=/=-3"
    (run* (q)
      (fresh (x y)
        (=/= x y)
        (== x y)))
    '())
  (test "=/=-4"
    (run* (q)
      (fresh (x y)
        (== x y)
        (=/= x y)))
    '())
  (test "=/=-5"
    (run* (q)
      (fresh (x y)
        (=/= x y)
        (== 3 x)
        (== 3 y)))
    '())
  (test "=/=-6"
    (run* (q)
      (fresh (x y)
        (== 3 x)
        (=/= x y)
        (== 3 y)))
    '())
  (test "=/=-7"
    (run* (q)
      (fresh (x y)
        (== 3 x)
        (== 3 y)
        (=/= x y)))
    '())
  (test "=/=-8"
    (run* (q)
      (fresh (x y)
        (== 3 x)
        (== 3 y)
        (=/= y x)))
    '())
  (test "=/=-9"
    (run* (q)
      (fresh (x y z)
        (== x y)
        (== y z)
        (=/= x 4)
        (== z (+ 2 2))))
    '())
  (test "=/=-10"
    (run* (q)
      (fresh (x y z)
        (== x y)
        (== y z)
        (== z (+ 2 2))
        (=/= x 4)))
    '())

  (test "=/=-11"
    (run* (q)
      (fresh (x y z)
        (=/= x 4)
        (== y z)
        (== x y)
        (== z (+ 2 2))))
    '())
  (test "=/=-12"
    (run* (q)
      (fresh (x y z)
        (=/= x y)
        (== x `(0 ,z 1))
        (== y `(0 1 1))))
    '((_.0)))
  (test "=/=-13"
    (run* (q)
      (fresh (x y z)
        (=/= x y)
        (== x `(0 ,z 1))
        (== y `(0 1 1))
        (== z 1)
        (== `(,x ,y) q)))
    '())
  (test "=/=-14"
    (run* (q)
      (fresh (x y z)
        (=/= x y)
        (== x `(0 ,z 1))
        (== y `(0 1 1))
        (== z 0)))
    '((_.0)))
  (test "=/=-15"
    (run* (q)
      (fresh (x y z)
        (== z 0)
        (=/= x y)
        (== x `(0 ,z 1))
        (== y `(0 1 1))))
    '((_.0)))
  (test "=/=-16"
    (run* (q)
      (fresh (x y z)
        (== x `(0 ,z 1))
        (== y `(0 1 1))
        (=/= x y)))
    '((_.0)))
  (test "=/=-17"
    (run* (q)
      (fresh (x y z)
        (== z 1)
        (=/= x y)
        (== x `(0 ,z 1))
        (== y `(0 1 1))))
    '())
  (test "=/=-18"
    (run* (q)
      (fresh (x y z)
        (== z 1)
        (== x `(0 ,z 1))
        (== y `(0 1 1))
        (=/= x y)))
    '())
  (test "=/=-19"
    (run* (q)
      (fresh (x y)
        (=/= `(,x 1) `(2 ,y))
        (== x 2)))
    '((_.0)))
  (test "=/=-20"
    (run* (q)
      (fresh (x y)
        (=/= `(,x 1) `(2 ,y))
        (== y 1)))
    '((_.0)))

  (test "=/=-21"
    (run* (q)
      (fresh (x y)
        (=/= `(,x 1) `(2 ,y))
        (== x 2)
        (== y 1)))
    '())
  (test "=/=-24"
    (run* (q)
      (fresh (x y)
        (=/= `(,x 1) `(2 ,y))
        (== x 2)
        (== y 9)
        (== `(,x ,y) q)))
    '(((2 9))))
  (test "=/=-24b"
    (run* (q)
      (fresh (a d)
        (== `(,a . ,d) q)
        (=/= q `(5 . 6))
        (== a 5)
        (== d 6)))
    '())
  (test "=/=-25"
    (run* (q)
      (fresh (x y)
        (=/= `(,x 1) `(2 ,y))
        (== x 2)
        (== y 1)
        (== `(,x ,y) q)))
    '())
  (test "=/=-26"
    (run* (q)
      (fresh (a x z)
        (=/= a `(,x 1))
        (== a `(,z 1))
        (== x z)))
    '())
  (test "=/=-28"
    (run* (q)
      (=/= 3 4))
    '((_.0)))
  (test "=/=-29"
    (run* (q)
      (=/= 3 3))
    '())
  (test "=/=-30"
    (run* (q) (=/= 5 q)
      (=/= 6 q)
      (== q 5))
    '())

  (test "=/=-32"
    (run* (q)
      (fresh (a)
        (== 3 a)
        (=/= a 4)))
    '((_.0)))
  (test "=/=-35"
    (let ((foo (lambda (x)
                (fresh (a)
                  (=/= x a)))))
      (run* (q) (fresh (a) (foo a))))
    '((_.0)))
  (test "=/=-36"
    (let ((foo (lambda (x)
                (fresh (a)
                  (=/= x a)))))
      (run* (q) (fresh (b) (foo b))))
    '((_.0)))
  (test "=/=-37c"
    (run* (q)
    (fresh (a d)
      (== `(,a . ,d) q)
      (=/= q `(5 . 6))
      (== a 3)))
    '(((3 . _.0))))
  (test "=/=-47"
    (run* (x)
      (fresh (y z)
        (=/= x `(,y 2))
        (== x `(,z 2))))
    '(((_.0 2))))
  (test "=/=-48"
    (run* (x)
      (fresh (y z)
        (=/= x `(,y 2))
        (== x `((,z) 2))))
    '((((_.0) 2))))
  (test "=/=-49"
    (run* (x)
      (fresh (y z)
        (=/= x `((,y) 2))
        (== x `(,z 2))))
    '(((_.0 2))))

  (test "numbero-2"
    (run* (q) (numbero q) (== 5 q))
    '((5)))
  (test "numbero-3"
    (run* (q) (== 5 q) (numbero q))
    '((5)))
  (test "numbero-4"
    (run* (q) (== 'x q) (numbero q))
    '())
  (test "numbero-5"
    (run* (q) (numbero q) (== 'x q))
    '())
  (test "numbero-6"
    (run* (q) (numbero q) (== `(1 . 2) q))
    '())
  (test "numbero-7"
    (run* (q) (== `(1 . 2) q) (numbero q))
    '())
  (test "numbero-8"
    (run* (q) (fresh (x) (numbero x)))
    '((_.0)))
  (test "numbero-9"
    (run* (q) (fresh (x) (numbero x)))
    '((_.0)))
  (test "numbero-14-b"
    (run* (q) (fresh (x) (numbero q) (== 5 x) (== x q)))
    '((5)))
  (test "numbero-15"
    (run* (q) (fresh (x) (== q x) (numbero q) (== 'y x)))
    '())
  (test "numbero-24-a"
    (run* (q)
      (fresh (w x y z)
        (=/= `(,w . ,x) `(,y . ,z))
        (numbero w)
        (numbero z)))
    '((_.0)))

  (test "symbolo-2"
    (run* (q) (symbolo q) (== 'x q))
    '((x)))
  (test "symbolo-3"
    (run* (q) (== 'x q) (symbolo q))
    '((x)))
  (test "symbolo-4"
    (run* (q) (== 5 q) (symbolo q))
    '())
  (test "symbolo-5"
    (run* (q) (symbolo q) (== 5 q))
    '())
  (test "symbolo-6"
    (run* (q) (symbolo q) (== `(1 . 2) q))
    '())
  (test "symbolo-7"
    (run* (q) (== `(1 . 2) q) (symbolo q))
    '())
  (test "symbolo-8"
    (run* (q) (fresh (x) (symbolo x)))
    '((_.0)))
  (test "symbolo-9"
    (run* (q) (fresh (x) (symbolo x)))
    '((_.0)))
  (test "symbolo-14-b"
    (run* (q) (fresh (x) (symbolo q) (== 'y x) (== x q)))
    '((y)))
  (test "symbolo-15"
    (run* (q) (fresh (x) (== q x) (symbolo q) (== 5 x)))
    '())
  (test "symbolo-24-a"
    (run* (q)
      (fresh (w x y z)
        (=/= `(,w . ,x) `(,y . ,z))
        (symbolo w)
        (symbolo z)))
    '((_.0)))

  (test "symbolo-numbero-1"
    (run* (q) (symbolo q) (numbero q))
    '())
  (test "symbolo-numbero-2"
    (run* (q) (numbero q) (symbolo q))
    '())
  (test "symbolo-numbero-3"
    (run* (q)
      (fresh (x)
        (numbero x)
        (symbolo x)))
    '())
  (test "symbolo-numbero-4"
    (run* (q)
      (fresh (x)
        (symbolo x)
        (numbero x)))
    '())
  (test "symbolo-numbero-5"
    (run* (q)
      (numbero q)
      (fresh (x)
        (symbolo x)
        (== x q)))
    '())
  (test "symbolo-numbero-6"
    (run* (q)
      (symbolo q)
      (fresh (x)
        (numbero x)
        (== x q)))
    '())
  (test "symbolo-numbero-7"
    (run* (q)
      (fresh (x)
        (numbero x)
        (== x q))
      (symbolo q))
    '())
  (test "symbolo-numbero-7"
    (run* (q)
      (fresh (x)
        (symbolo x)
        (== x q))
      (numbero q))
    '())
  (test "symbolo-numbero-8"
    (run* (q)
      (fresh (x)
        (== x q)
        (symbolo x))
      (numbero q))
    '())
  (test "symbolo-numbero-9"
    (run* (q)
      (fresh (x)
        (== x q)
        (numbero x))
      (symbolo q))
    '())

  (test "symbolo-numbero-32"
    (run* (q)
      (fresh (x y)
        (=/= `(,x ,y) q)
        (numbero x)
        (symbolo y)))
    '((_.0)))
  (test "symbolo-numbero-33"
    (run* (q)
      (fresh (x y)
        (numbero x)
        (=/= `(,x ,y) q)
        (symbolo y)))
    '((_.0)))
  (test "symbolo-numbero-34"
    (run* (q)
      (fresh (x y)
        (numbero x)
        (symbolo y)
        (=/= `(,x ,y) q)))
    '((_.0)))

  (test "test 24"
    (run* (q) (== 5 q) (absento 5 q))
    '())
  (test "test 25"
    (run* (q) (== q `(5 6)) (absento 5 q))
    '())
  (test "test 25b"
    (run* (q) (absento 5 q) (== q `(5 6)))
    '())
  (test "test 26"
    (run* (q) (absento 5 q) (== 5 q))
    '())
  (test "test 33"
    (run* (q)
      (fresh (a b c)
        (== `(,a ,b) c)
        (== `(,c ,c) q)
        (symbolo b)
        (numbero c)))
    '())
  (test "test 40"
    (run* (q)
      (fresh (d a c)
        (== `(3 . ,d) q)
        (=/= `(,c . ,a) q)
        (== '(3 . 4) d)))
    '(((3 3 . 4))))
  (test "test 41"
    (run* (q)
      (fresh (a)
        (== `(,a . ,a) q)))
    '(((_.0 . _.0))))
  (test "test 63"
    (run* (q) (fresh (a b c) (=/= a b) (=/= b c) (=/= c q) (symbolo a)))
    '((_.0)))
  (test "test 64"
    (run* (q) (symbolo q) (== 'tag q))
    '((tag)))
  (test "test 66"
    (run* (q) (absento 6 5))
    '((_.0)))

  (test "absento 'closure-1a"
    (run* (q) (absento 'closure q) (== q 'closure))
    '())
  (test "absento 'closure-1b"
    (run* (q) (== q 'closure) (absento 'closure q))
    '())
  (test "absento 'closure-2a"
    (run* (q) (fresh (a d) (== q 'closure) (absento 'closure q)))
    '())
  (test "absento 'closure-2b"
    (run* (q) (fresh (a d) (absento 'closure q) (== q 'closure)))
    '())
  (test "absento 'closure-4a"
    (run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure a)))
    '())
  (test "absento 'closure-4b"
    (run* (q) (fresh (a d) (absento 'closure q) (== 'closure a) (== `(,a . ,d) q)))
    '())
  (test "absento 'closure-4c"
    (run* (q) (fresh (a d) (== 'closure a) (absento 'closure q) (== `(,a . ,d) q)))
    '())
  (test "absento 'closure-4d"
    (run* (q) (fresh (a d) (== 'closure a) (== `(,a . ,d) q) (absento 'closure q)))
    '())
  (test "absento 'closure-5a"
    (run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure d)))
    '())
  (test "absento 'closure-5b"
    (run* (q) (fresh (a d) (absento 'closure q) (== 'closure d) (== `(,a . ,d) q)))
    '())
  (test "absento 'closure-5c"
    (run* (q) (fresh (a d) (== 'closure d) (absento 'closure q) (== `(,a . ,d) q)))
    '())
  (test "absento 'closure-5d"
    (run* (q) (fresh (a d) (== 'closure d) (== `(,a . ,d) q) (absento 'closure q)))
    '())
  (test "absento 'closure-6"
    (run* (q)
      (== `(3 (closure x (x x) ((y . 7))) #t) q)
      (absento 'closure q))
    '())
  )

(define (letrec-append body)
    `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       ,body))

(module+ test
  (define-syntax test-eval
    (syntax-rules ()
     ((_ tm result)
      (let ((tm0 tm))
        (check-true (term? tm0 initial-env))
        (check-equal? (run 1 (answer) (dk-evalo tm0 answer))
                      (list (list result)))))))
  (test-eval 3 3)
  (test-eval '3 3)
  (test-eval ''x 'x)
  (test-eval ''(1 (2) 3) '(1 (2) 3))
  (test-eval '(car '(1 (2) 3)) 1)
  (test-eval '(cdr '(1 (2) 3)) '((2) 3))
  (test-eval '(cons 'x 4) '(x . 4))
  (test-eval '(null? '()) #t)
  (test-eval '(null? '(0)) #f)
  (test-eval '(list 5 6) '(5 6))
  (test-eval '(and 8 9) 9)
  (test-eval '(and #f 9 10) #f)
  (test-eval '(and #f (letrec ((loop (lambda (x) (loop x))))
                        (loop 'forever))) #f)
  (test-eval '(and 8 9 10) 10)
  (test-eval '(or #f 11 12) 11)
  (test-eval '(or #t (letrec ((loop (lambda (x) (loop x))))
                       (loop 'forever))) #t)
  (test-eval '(let ((p (cons 8 9))) (cdr p)) 9)

  (define (letrec-append body)
    `(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       ,body))
  (define ex-append
    (letrec-append
      '(list (append '() '()) (append '(foo) '(bar)) (append '(1 2) '(3 4)))))
  (define ex-append-answer '(() (foo bar) (1 2 3 4)))
  (test-eval ex-append ex-append-answer)

  (test-eval '`(1 ,(car `(,(cdr '(b 2)) 3)) ,'a) '(1 (2) a))

  (test-eval
    '(match '(1 (b 2))
       (`(1 (a ,x)) 3)
       (`(1 (b ,x)) x)
       (_ 4))
    2)
  (test-eval
    '(match '(1 1 2)
       (`(,a ,b ,a) `(first ,a ,b))
       (`(,a ,a ,b) `(second ,a ,b))
       (_ 4))
    '(second 1 2))

  (define ex-match
    '(match '(1 2 1)
       (`(,a ,b ,a) `(first ,a ,b))
       (`(,a ,a ,b) `(second ,a ,b))
       (_ 4)))
  (test-eval ex-match '(first 1 2))

  (test-eval
    '(match '(1 b 3)
       ((or `(0 ,x ,y) `(1 ,x ,y)) 'success)
       (_ 'fail))
    'success)

  (test-eval
    '(match '(1 b 3)
       ((or `(0 ,x ,y) `(1 ,y ,x)) 'success)
       (_ 'fail))
    'success)

  (test-eval
    '(match '(0 b 3)
       ((or `(0 ,x ,y) `(1 ,y ,x)) 'success)
       (_ 'fail))
    'success)

  (define-syntax run-det
    (syntax-rules ()
      ((_ n (qv ...) goal ...)
       (map (reify var-0)
            (take n (zzz ((fresh (qv ...)
                            (== (list qv ...) var-0) goal ...
                            (lambda (st) (reset-cost (state-resume-det1 st))))
                          state-empty)))))))
  (define-syntax run*-det
    (syntax-rules () ((_ body ...) (run-det #f body ...))))

  (define-syntax test-dk-evalo
    (syntax-rules ()
     ((_ tm result)
      (let ((tm0 tm) (res0 result))
        (when (not (term? tm0 initial-env))
          (error (format "not a term: ~a" tm0)))
        (dk-evalo tm0 res0)))))

  (test "match-simple-0"
    (run* (q r)
      (test-dk-evalo
        `(match ',q)
        r))
    '())
  (test "match-simple-1"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (_ #t))
        r))
    '((_.0 #t)))
  (test "match-simple-2"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (x x)
           (_ #t))
        r))
    '((_.0 _.0)))
  (test "match-simple-3"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((not x) #f)
           (_ #t))
        r))
    '((_.0 #t)))
  (test "match-simple-4"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (_ #f)
           (_ #t))
        r))
    '((_.0 #f)))
  (test "match-simple-5"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((not _) #f)
           (_ #t))
        r))
    '((_.0 #t)))

  (test "match-simple-6"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           (12 'twelve)
           (#t 'true)
           (#f 'false)
           ('sym 'symbol)
           ('() 'nil)
           ('(a b) 'pair))
        r))
    '((8 eight)
      (12 twelve)
      (#t true)
      (#f false)
      (sym symbol)
      (() nil)
      ((a b) pair)))

  (test "match-simple-7"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           (12 'twelve)
           (#t 'true)
           (_ (match ',q
                (#f 'false)
                ('sym 'symbol)
                ('() 'nil)
                ('(a b) 'pair))))
        r))
    '((8 eight)
      (12 twelve)
      (#t true)
      (#f false)
      (sym symbol)
      (() nil)
      ((a b) pair)))

  (test "match-simple-8"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           (12 'twelve)
           (#t 'true)
           (8 (match ',q
                (#f 'false)
                ('sym 'symbol)
                ('() 'nil)
                ('(a b) 'pair))))
        r))
    '((8 eight) (12 twelve) (#t true)))

  (test "match-simple-9"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           ((number) 'number)
           ((symbol) 'symbol)
           (12 'twelve)
           (#t 'true)
           (`(,x ,y . ,z) 'pair-2)
           (`(,x . ,y) 'pair)
           (`(,w ,x ,y . ,z) 'pair-3)
           (#f 'false)
           ('sym 'symbol)
           ('() 'nil)
           ('(a b) 'pair)
           (_ 'anything))
        r))
    '((8 eight)
      (_.0 number)
      (_.0 symbol)
      (#t true)
      ((_.0 _.1 . _.2) pair-2)
      ((_.0 . _.1) pair)
      (#f false)
      (() nil)))

  (test "match-simple-10"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           ((number) 'number)
           ((symbol) 'symbol)
           (12 'twelve)
           (#t 'true)
           (`(,x ,y . ,z) 'pair-2)
           (`(,x . ,y) 'pair)
           (`(,w ,x ,y . ,z) 'pair-3)
           (#f 'false)
           ('sym 'symbol)
           ('(a b) 'pair)
           (_ 'anything))
        r))
    '((8 eight)
      (_.0 number)
      (_.0 symbol)
      (#t true)
      ((_.0 _.1 . _.2) pair-2)
      ((_.0 . _.1) pair)
      (#f false)
      (() anything)))

  (test "match-simple-11"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           ((number) 'number)
           ((symbol) 'symbol)
           (12 'twelve)
           (#t 'true)
           (`(,x ,y . ,z) 'pair-2)
           (`(,x . ,y) 'pair)
           (`(,w ,x ,y . ,z) 'pair-3)
           ('sym 'symbol)
           ('(a b) 'pair)
           ((not '()) 'anything))
        r))
    '((8 eight)
      (_.0 number)
      (_.0 symbol)
      (#t true)
      ((_.0 _.1 . _.2) pair-2)
      ((_.0 . _.1) pair)
      (#f anything)))

  (test "match-simple-12"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (8 'eight)
           ((number) 'number)
           ((symbol) 'symbol)
           (12 'twelve)
           (#t 'true)
           (`(,x ,y . ,z) 'pair-2)
           (`(,x . ,y) 'pair)
           (`(,w ,x ,y . ,z) 'pair-3)
           ('sym 'symbol)
           ('(a b) 'pair)
           ((not '()) 'anything))
        r))
    '((8 eight)
      (_.0 number)
      (_.0 symbol)
      (#t true)
      ((_.0 _.1 . _.2) pair-2)
      ((_.0 . _.1) pair)
      (#f anything)))

  (test "match-compound-1"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((and #t #f #t) 'impossible))
        r))
    '())
  (test "match-compound-2"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((and #t #t #t) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t possible) (#f false)))
  (test "match-compound-3"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((or #t #f #t) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t possible) (#f possible)))
  (test "match-compound-4"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((not (or #t #f #t)) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((_.0 possible) (#t true) (#f false)))
  (test "match-compound-5"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((and (not (or #t #f #t)) #t) 'impossible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t true) (#f false)))
  (test "match-compound-6"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((not (not (and #t #t #t))) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t possible) (#f false)))
  (test "match-compound-7"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((not (not (and #t (not #t) #t))) 'impossible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t true) (#f false)))

  (test "match-compound-8"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((or #t #f #t 5) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((#t possible) (#f possible) (5 possible)))
  (test "match-compound-8b"
    (run* (q r)
      (=/= #t q)
      (test-dk-evalo
        `(match ',q
           ((or #t #f #t 5) 'possible)
           (#t 'true)
           (#f 'false))
        r))
    '((#f possible) (5 possible)))
  (test "match-compound-8c"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((or #t #f #t 5) 'possible)
           (#t 'true)
           (#f 'false))
        r)
      (=/= #t q))
    '((#f possible) (5 possible)))

  (test "match-qq-1"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (`(,(and 1 1 1) . ,(and 2 2 2)) 'ok))
        r))
    '(((1 . 2) ok)))
  (test "match-qq-2a"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (`(,(and 1 (not 1) 1) . ,(and 2 2 2)) 'ok))
        r))
    '())
  (test "match-qq-2b"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (`(,(and 1 (not 2) 1) . ,(and 2 2 2)) 'ok))
        r))
    '(((1 . 2) ok)))
  (test "match-qq-3"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (`(,(and 1 x 1) . ,(and 2 y 2)) 'ok))
        r))
    '(((1 . 2) ok)))
  (test "match-qq-4"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           (`(,(and 1 x 1) . ,(and 2 x 2)) 'ok))
        r))
    '())
  (test "match-qq-5"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((and `(1 . ,x) `(,y . 2)) 'ok))
        r))
    '(((1 . 2) ok)))
  (test "match-qq-6"
    (run* (q r)
      (test-dk-evalo
        `(match ',q
           ((and `(1 . ,x) `(,x . 2)) 'ok))
        r))
    '())

  (test "match-?-0"
    (run* (q r)
      (test-dk-evalo
        `(let ((ok? (lambda (x) #f)))
           (match ',q
             ((? ok?) 'ok)))
        r))
    '())
  (test "match-?-1"
    (run* (q r)
      (test-dk-evalo
        `(let ((ok? (lambda (x) #t)))
           (match ',q
             ((? ok?) 'ok)))
        r))
    '((_.0 ok)))
  (test "match-?-2"
    (run* (q r)
      (test-dk-evalo
        `(let ((ok? (lambda (x) #f)))
           (match ',q
             ((? ok?) 'ok)
             (3 'three)))
        r))
    '((3 three)))
  (test "match-?-4"
    (run* (q r)
      (test-dk-evalo
        `(let ((number? (lambda (x) (match x
                                      ((number) #t)
                                      (_ #f)))))
           (match ',q
             ((? number?) 'ok)))
        r))
    '((_.0 ok)))
  (test "match-?-5"
    (run* (q r)
      (test-dk-evalo
        `(let ((number? (lambda (x) (match x
                                      ((number) #t)
                                      (_ #f)))))
           (match ',q
             ((? number?) 'ok)
             (4 'four)
             (#t 'true)
             ))
        r))
    '((_.0 ok) (#t true)))

  (test "match-match-0"
    (run* (q r)
      (test-dk-evalo
        `(match (match ',q
                  ('a 15)
                  ('b 16)
                  ('c 19)
                  ('d 16)
                  ('e 15)
                  ('f 14))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        r))
    '())
  (test "match-match-1"
    (run* (q r)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 6)
                  ('e 5)
                  ('f 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        r))
    '((a 2) (b 3) (d 3) (e 2) (f 1)))
  (test "match-match-2"
    (run* (q)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 6)
                  ('e 5)
                  ('f 7)
                  ('g 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        2))
    '((a) (e) (f)))
  (test "match-match-3"
    (run* (q r)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 6)
                  ('e 5)
                  ('f 7)
                  ('g 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        r)
      (== 2 r))
    '((a 2) (e 2) (f 2)))
  (test "match-match-4"
    (run* (q r)
      (== 2 r)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 6)
                  ('e 5)
                  ('f 7)
                  ('g 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        r))
    '((a 2) (e 2) (f 2)))

  (test "match-match-det-1"
    (run*-det (q)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 8)
                  ('d 6)
                  ('e 5)
                  ('f 7)
                  ('g 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        3))
    '((b)))
  (test "match-match-det-2"
    (run*-det (q r)
      (test-dk-evalo
        `(match (match ',q
                  ('a 5)
                  ('b 6)
                  ('c 9)
                  ('d 8)
                  ('d 6)
                  ('e 5)
                  ('f 7)
                  ('g 4))
           (4 1)
           (5 2)
           (6 3)
           (7 2)
           (8 1))
        r)
      (== r 3))
    '((b 3)))

  (test "append-deterministic-1"
    (run*-det (q)
      (test-dk-evalo (letrec-append `(append '(1 2 3) ',q)) '(1 2 3 4 5)))
    '(((4 5))))
  (test "append-deterministic-2"
    (run*-det (q)
      (fresh (a b c) (== `(,a ,b ,c) q))
      (test-dk-evalo (letrec-append `(append ',q '(4 5))) '(1 2 3 4 5)))
    '(((1 2 3))))
  (test "append-deterministic-3"
    (run*-det (q)
      (test-dk-evalo (letrec-append `(append ',q '(4 5))) '(1 2 3 4 5))
      (fresh (a b c) (== `(,a ,b ,c) q)))
    '(((1 2 3))))

  (test "append-nondet-1"
    (run* (q)
      (test-dk-evalo (letrec-append `(append ',q '(4 5))) '(1 2 3 4 5)))
    '(((1 2 3))))
  (test "append-nondet-2"
    (run* (q r)
      (test-dk-evalo (letrec-append `(append ',q ',r)) '(1 2 3 4 5)))
    '((() (1 2 3 4 5))
      ((1) (2 3 4 5))
      ((1 2) (3 4 5))
      ((1 2 3) (4 5))
      ((1 2 3 4) (5))
      ((1 2 3 4 5) ())))
  )
