#lang racket/base
(provide
  term?
  eval-term
  initial-env
  )

(require
  racket/list
  racket/match
  racket/set
  )

(module+ test
  (require
    rackunit
    ))

; translate the same output to both direct and goal interpretations
;
; match/r
;
; optional match result-domain annotations
;   type or finite domain: support fast, conservative domain constraints
;     domain = _ (anything), ([quasi]quoted) literal, infinite set (i.e. number, symbol), list (union) of domains
;   last-of-this-value markers: if you have this value, it's the last case where it's possible, so commit to it

; TODO:
; basic mk variant
;   ==, =/=, type-based versions of these
;   bind, mplus w/ costs
;     dfs, ws, quota/cost?
;     cost must be able to express size-incrementing search
;       goal scoped or leaky costs?
;       quota wrapper?
;   simple take, reify, run, for debugging
; extended mk variant, mixing in deterministic computation where possible
;   match/pattern compiler
;     backwards result value information flow
;       match clause rhs can often be treated as a pattern
;       track a pair of values that distinguish these cases:
;         [partly-]instantiated datum: #f, datum
;           apply backwards pattern matching where helpful
;         existing var w or w/o cxs: var, cxs/#f
;           if goal doesn't produce a result, bind to existing logic var
;         no value, no existing var: #t, cxs/#f
;           if goal doesn't produce a result, create and bind to a fresh logic var
;           is there really ever a cxs in this case?
;   non-root-deterministic-only quotas
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
; faster-mk interface
;   port everything to chez
;   import/export substitution and constraint store, translating constraints
; performance and benchmarking
;   move from closure-encoding interpreter to ahead-of-time compiler
;   tweak match clauses and costs
;   tweak mk data reprs
;     e.g. try path compression
; future
;   de bruijn encoding interpreter
;     could be a useful alternative to closure-encoding for 1st order languages
;     try for both dkanren (for comparison) and relational interpreter (may improve perf)

(define-syntax defrec
  (syntax-rules ()
    ((_ name fnames ...) (struct name (fnames ...) #:transparent))))

(define-syntax let*/and
  (syntax-rules ()
    ((_ ((name expr) ...) rest ...)
     (let* ((name expr) ...) (and name ... rest ...)))))

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
(define (store-key-top store)
  (let ((keys (hash-keys store))) (and (pair? keys) (car keys))))
(define (list-add-unique xs v) (if (memq v xs) xs (cons v xs)))
(define (list-append-unique xs ys)
  (if (null? xs) ys
    (let ((zs (list-append-unique (cdr xs) ys))
          (x0 (car xs)))
      (if (memq x0 ys) zs (cons x0 zs)))))

(defrec var name)
(define var-0 (var 'initial))

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
(define (vattr-=/=s-add va val)
  (if (or (var? val) (domain-has-val? (vattr-domain va) val))
    (vattr (vattr-domain va)
           (list-add-unique (vattr-=/=s va) val)
           (vattr-goals va)
           (vattr-dependencies va))
    va))
(define (vattr-=/=s-has? va val) (memq val (vattr-=/=s va)))
(define (vattr-associate va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (cons goal (vattr-goals va))
         (vattr-dependencies va)))
(define (vattr-depend va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (cons goal (vattr-goals va))
         (cons goal (vattr-dependencies va))))
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

(defrec goal-suspended tag result blockers retry guess)
(define (goal-ref-new) (gensym))
(define (goal-retry goals goal)
  (if (procedure? goal) goal
    (let*/and ((gsusp (store-ref goals goal)))
      (goal-suspended-retry gsusp))))

(define (goal-block-cons block blocks)
  (if (null? block) blocks (cons block blocks)))
(defrec schedule det det-deferred nondet)
(define schedule-empty (schedule '() '() '()))
(define (schedule-add-det sch det)
  (schedule (goal-block-cons det (schedule-det sch))
            (schedule-det-deferred sch)
            (schedule-nondet sch)))
(define (schedule-add sch det nondet)
  (schedule (goal-block-cons det (schedule-det sch))
            (schedule-det-deferred sch)
            (goal-block-cons nondet (schedule-nondet sch))))
(define (schedule-defer sch goal)
  (schedule (schedule-det sch)
            (cons goal (schedule-det-deferred sch))
            (schedule-nondet sch)))

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
(define (state-suspend* st var-forwards var-backwards goal)
  (let* ((goal-ref (goal-ref-new))
         (goals (store-set (state-goals st) goal-ref goal))
         (vs (state-vs st))
         (vs (foldl (lambda (vr vs)
                      (vattrs-set vs vr (vattr-associate
                                          (vattrs-get vs vr) goal-ref)))
                    vs var-forwards))
         (vs (foldl (lambda (vr vs)
                      (vattrs-set vs vr (vattr-depend
                                          (vattrs-get vs vr) goal-ref)))
                    vs var-backwards)))
    (state vs goals (state-schedule st))))
(define (state-resume st)
  (let ((sch (state-schedule st)))
    (let/list loop ((goals det (schedule-det sch))
                    (det-def (schedule-det-deferred sch))
                    (nondet (schedule-nondet sch)))
      (let/list loop1 ((goal goals goals))
        (let/if (retry (goal-retry (state-goals st) goal))
          (retry (state (state-vs st)
                        (state-goals st)
                        (schedule (goal-block-cons goals det) det-def nondet)))
          (loop1 goals))
        (loop det det-def nondet))
      (if (pair? det-def)
        (loop (list det-def) '() nondet)
        (let/list loop ((goals nondet nondet) (vs (state-vs st)))
          (let/list loop1 ((goal goals1 goals))
            (let/if (gsusp (store-ref (state-goals st) goal #f))
              (let/list loop2
                ((blocker blockers (goal-suspended-blockers gsusp)))
                (let-values (((blocker va) (walk-vs vs blocker)))
                  (let ((deps (vattr-dependencies va)))
                    (if (null? deps)
                      (loop2 blockers)
                      (loop
                        (cons deps (cons goals nondet))
                        (vattrs-set vs blocker (vattr-dependencies-clear va))))))
                ((goal-suspended-guess gsusp)
                 (state vs
                        (store-remove (state-goals st) goal)
                        (schedule '() '() (goal-block-cons goals1 nondet)))))
              (loop1 goals1))
            (loop nondet vs))
          (let/if (goal-ref (store-key-top (state-goals st)))
            (loop (list (list goal-ref)) vs)
            st))))))

(define (state-var-type-== st vr va type)
  (and (domain-has-type? (vattr-domain va) type)
       (state-var-set st vr (vattr-domain-set va `(,type)))))
(define (state-var-type-=/= st vr va type)
  (match (domain-remove (vattr-domain va) type)
    (`(,(and (not 'symbol) (not 'number) singleton))
      (state-var-== st vr va (if (eq? 'pair singleton)
                               `(,(var 'pair-a) . ,(var 'pair-d)) singleton)))
    ('() #f)
    (dmn (state-var-set st vr (vattr-domain-set va dmn)))))

;; Ideally we would notify any vars in =/=s that they can drop 'vr' from their
;; own =/=s, but not doing so shouldn't be a big deal.  Same story when
;; handling state-var-==-var.
(define (state-var-== st vr va val)
  (cond
    ((eq? vattr-empty va) (state-var-set st vr val))
    ((domain-has-val? (vattr-domain va) val)
     (let ((=/=s (vattr-=/=s va)))
       ; TODO: perf: combine memq with filter
       (and (not (memq val =/=s))
            (let ((vps (filter (if (pair? val)
                                 (lambda (x) (or (var? x) (pair? x)))
                                 var?) =/=s))
                  (st (state (store-set (state-vs st) vr val)
                             (state-goals st)
                             (schedule-add (state-schedule st)
                                           (vattr-goals va)
                                           (vattr-dependencies va)))))
              (disunify* st val vps)))))
     (else #f)))
(define (state-var-=/= st vr va val)
  (if (or (eq? '() val) (eq? #f val) (eq? #t val))
    (state-var-type-=/= st vr va val)
    (state-var-set st vr (vattr-=/=s-add va val))))
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
    (state-var-set (state-var-set st vr1 (vattr-=/=s-add va1 vr2))
                   vr2 (vattr-=/=s-add va2 vr1))
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

(define (succeed st) st)
(define (fail st) #f)

(define (== t0 t1) (lambda (st) (unify st t0 t1)))
(define (=/= t0 t1) (lambda (st) (disunify st t0 t1)))
(define (symbolo tm) (lambda (st) (typify st 'symbol tm)))
(define (numbero tm) (lambda (st) (typify st 'number tm)))
(define (not-symbolo tm) (lambda (st) (distypify st 'symbol tm)))
(define (not-numbero tm) (lambda (st) (distypify st 'number tm)))
(define (not-pairo tm) (lambda (st) (distypify st 'pair tm)))

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

(define-syntax bind
  (syntax-rules ()
    ((_) succeed)
    ((_ goal) goal)
    ((_ goal0 goal ...)
     (lambda (st) (let*/and ((st (goal0 st))) ((bind goal ...) st))))))

(define-syntax let/vars
  (syntax-rules ()
    ((_ (vname ...) body ...)
     (let ((vname (var 'vname)) ...) body ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (vname ...) goal ...) (let/vars (vname ...) (bind goal ...)))))

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
(define (denote-lambda params body senv)
  (match params
    ((and (not (? symbol?)) params)
     (let ((body (denote-term body (extend-env* params params senv))))
       (lambda (env) (goal-value (lambda args (body (rev-append args env)))))))
    (sym
      (let ((body (denote-term body `((val . (,sym . ,sym)) . senv))))
        (lambda (env) (goal-value (lambda args (body (cons args env)))))))))
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
                (loop-rec (+ ridx 1) binding*)))))))))

(define (denote-qq qqterm senv)
  (match qqterm
    (`(,'unquote ,term) (denote-term term senv))
    (`(,a . ,d) (let ((da (denote-qq a senv)) (dd (denote-qq d senv)))
                  (lambda (env)
                    (let ((ga (da env)) (gd (dd env)))
                      (lambda (st)
                        (let*/state (((st va) (ga st)) ((st vd) (gd st)))
                          (values st `(,va . ,vd))))))))
    ((? quotable? datum) (denote-value datum))))
(define (denote-and t* senv)
  (match t*
    ('() (denote-value #t))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let ((d0 (denote-term t senv)) (d* (denote-and t* senv)))
        (lambda (env)
          (let ((g0 (d0 env)) (g* (d* env)))
            (lambda (st)
              (let*/state (((st v0) (g0 st)))
                (if v0 (g* st) (values st #f))))))))))
(define (denote-or t* senv)
  (match t*
    ('() (denote-value #f))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let ((d0 (denote-term t senv)) (d* (denote-or t* senv)))
        (lambda (env)
          (let ((g0 (d0 env)) (g* (d* env)))
            (lambda (st)
              (let*/state (((st v0) (g0 st)))
                (if v0 (values st v0) (g* st))))))))))
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
              (let*/state (((st val) ((car g*) st)))
                (loop st (cons val xs) (cdr g*))))))))))

(define (denote-rhs-pattern-unknown env st v) #t)
(define (denote-rhs-pattern-literal literal)
  (lambda (env st v) (equal? literal v)))
(define (denote-rhs-pattern-var vname senv)
  (let ((dv (denote-term vname senv)))
    (lambda (env st v) (equal? ((dv env) st) v))))
(define (denote-rhs-pattern-qq qq senv)
  (match qq
    (`(,'unquote ,pat) (denote-rhs-pattern pat senv))
    (`(,a . ,d)
      (let ((da (denote-rhs-pattern-qq a senv))
            (dd (denote-rhs-pattern-qq d senv)))
        (lambda (env st v) (and (da env st v) (dd env st v)))))
    ((? quotable? datum) (denote-rhs-pattern-literal datum))))
(define (denote-rhs-pattern pat senv)
  (match pat
    (`(quote ,(? quotable? datum)) (denote-rhs-pattern-literal datum))
    (`(quasiquote ,qq) (denote-rhs-pattern-qq qq senv))
    ((? symbol? vname) (denote-rhs-pattern-var vname senv))
    ((? quotable? datum) (denote-rhs-pattern-literal datum))
    (_ denote-rhs-pattern-unknown)))

(defrec pattern sat nsat assert nassert)

(define (pattern-assert-any st penv v) (values st penv))
(define (pattern-assert-none st penv v) (values #f #f))
(define denote-pattern-fail
  (let ((pat (pattern (lambda (st penv v) (values #f #f))
                      (lambda (st penv v) (values penv '()))
                      pattern-assert-none
                      pattern-assert-any)))
    (lambda (env) pat)))
(define denote-pattern-succeed
  (let ((pat (pattern (lambda (st penv v) (values penv '()))
                      (lambda (st penv v) (values #f #f))
                      pattern-assert-any
                      pattern-assert-none)))
    (lambda (env) pat)))

; TODO: walk v
(define (denote-pattern-literal literal penv)
  (values penv (let ((pat (pattern
                            (lambda (st penv v) (if (equal? literal v)
                                                  (values penv '())
                                                  (values #f #f)))
                            (lambda (st penv v) (if (equal? literal v)
                                                  (values #f #f)
                                                  (values penv '())))
                            ; TODO:
                            (lambda (st penv v) (values st penv))
                            ; TODO:
                            (lambda (st penv v) (values st penv)))))
                 (lambda (env) pat))))

(define pattern-var-extend
  (pattern (lambda (st penv v) (values (cons v penv) '()))
           (lambda (st penv v) (values #f #f))
           (lambda (st penv v) (values st (cons v penv)))
           (lambda (st penv v) (values #f #f))))
(define (denote-pattern-var-extend env) pattern-var-extend)
(define (denote-pattern-var b* vname penv)
  (let loop ((idx 0) (b* b*))
    (match b*
      ('() (values `((,vname ,vname) . ,penv) denote-pattern-var-extend))
      (`((,name ,x) . ,b*)
        (if (eq? name vname)
          (values penv
                  (let ((pat (pattern
                               (lambda (st penv v)
                                 (if (equal? (list-ref penv idx) v)
                                   (values penv '())
                                   ; TODO: var?
                                   (values #f #f)))
                               (lambda (st penv v)
                                 (if (equal? (list-ref penv idx) v)
                                   (values #f #f)
                                   ; TODO: var?
                                   (values penv '())))
                               ; TODO:
                               (lambda (st penv v) (values st penv))
                               (lambda (st penv v) (values st penv)))))
                    (lambda (env) pat)))
          (loop (+ idx 1) b*))))))

(define (denote-pattern-qq qqpattern penv senv)
  (match qqpattern
    (`(,'unquote ,pat) (denote-pattern pat penv senv))
    (`(,a . ,d)
      (let*-values (((penv da) (denote-pattern-qq a penv senv))
                    ((penv dd) (denote-pattern-qq d penv senv)))
        (values penv
                (lambda (env)
                  (let ((pa (da env)) (pd (dd env)))
                    (pattern
                      (lambda (st penv v)
                        (if (pair? v)
                          (let-values
                            (((pea svsa) ((pattern-sat pa) st penv (car v))))
                            (if svsa
                              ; TODO: union svsa
                              ((pattern-sat pd) st pea (cdr v))
                              (values #f #f)))
                          ; TODO: var?
                          (values #f #f)))
                      (lambda (st penv v)
                        (if (pair? v)
                          (let-values
                            (((pea svsa) ((pattern-nsat pa) st penv (car v))))
                            (if svsa
                              ; TODO: union svsa
                              ((pattern-nsat pd) st pea (cdr v))
                              (values penv '())))
                          ; TODO: var?
                          (values penv '())))
                      (lambda (st penv v)
                        ; TODO:
                        (values st penv))
                      (lambda (st penv v)
                        ; TODO:
                        (values st penv))))))))
    ((? quotable? datum) (denote-pattern-literal datum penv))))
(define (denote-pattern-or pattern* penv senv)
  (match pattern*
    ('() (values penv denote-pattern-fail))
    (`(,pat . ,pat*)
      (let*-values (((_ d0) (denote-pattern pat penv senv))
                    ((_ d*) (denote-pattern-or pat* penv senv)))
        (values penv
                (lambda (env)
                  (let ((p0 (d0 env)) (p* (d* env)))
                    (pattern
                      (lambda (st penv v)
                        (let-values
                          (((pe0 svs0) ((pattern-sat p0) st penv v)))
                          (if svs0
                            (values penv svs0)
                            ((pattern-sat p*) st penv v))))
                      (lambda (st penv v)
                        (let-values
                          (((pe0 svs0) ((pattern-nsat p0) st penv v)))
                          (if svs0
                            ; TODO: union svs0
                            ((pattern-nsat p*) st penv v)
                            (values #f #f))))
                      (lambda (st penv v)
                        ; TODO:
                        (values st penv))
                      (lambda (st penv v)
                        ; TODO:
                        (values st penv))))))))))

(define (denote-pattern* pattern* penv senv)
  (match pattern*
    ('() (values penv denote-pattern-succeed))
    (`(,pat . ,pat*)
      (let*-values (((penv d0) (denote-pattern pat penv senv))
                    ((penv d*) (denote-pattern* pat* penv senv)))
        (values penv
                (lambda (env)
                  (let ((p0 (d0 env)) (p* (d* env)))
                    (pattern
                      (lambda (st penv v)
                        (let-values
                          (((pe0 svs0) ((pattern-sat p0) st penv v)))
                          (if svs0
                            ; TODO: union svs0
                            ((pattern-sat p*) st pe0 v)
                            (values #f #f))))
                      (lambda (st penv v)
                        (let-values
                          (((pe0 svs0) ((pattern-nsat p0) st penv v)))
                          (if svs0
                            (values penv svs0)
                            ((pattern-nsat p*) st penv v))))
                      ; TODO:
                      (lambda (st penv v) (values st penv))
                      ; TODO:
                      (lambda (st penv v) (values st penv))))))))))

(define (denote-pattern-type type? pat* penv senv)
  (let-values (((penv d*) (denote-pattern* pat* penv senv)))
    (values penv
            (lambda (env)
              (let ((p* (d* env)))
                (pattern
                  (lambda (st penv v)
                    (if (type? v)
                      ((pattern-sat p*) st penv v)
                      ; TODO: var?
                      (values #f #f)))
                  (lambda (st penv v)
                    (if (type? v)
                      ((pattern-nsat p*) st penv v)
                      ; TODO: var?
                      (values penv '())))
                  ; TODO:
                  (lambda (st penv v) (values st penv))
                  ; TODO:
                  (lambda (st penv v) (values st penv))))))))

(define (denote-pattern pat penv senv)
  (match pat
    (`(quote ,(? quotable? datum)) (denote-pattern-literal datum penv))
    (`(quasiquote ,qqpat) (denote-pattern-qq qqpat penv senv))
    (`(not . ,pat*)
      (let-values (((_ dp) (denote-pattern* pat* penv senv)))
        (values penv
                (lambda (env)
                  (let ((pp (dp env)))
                    (pattern
                      (pattern-nsat pp)
                      (lambda (st penv v)
                        (let-values (((_ svs) ((pattern-sat pp) st penv v)))
                          (if svs
                            (values penv svs)
                            (values #f #f))))
                      (pattern-nassert pp)
                      (lambda (st penv v)
                        (let-values (((st _) ((pattern-assert pp) st penv v)))
                          (if st
                            (values st penv)
                            (values #f #f))))))))))
    (`(and . ,pat*) (denote-pattern* pat* penv senv))
    (`(or . ,pat*) (denote-pattern-or pat* penv senv))
    (`(symbol . ,pat*) (denote-pattern-type symbol? pat* penv senv))
    (`(number . ,pat*) (denote-pattern-type number? pat* penv senv))
    (`(? ,predicate . ,pat*)
      (let ((dpred (denote-term predicate senv)))
        (let-values (((penv d*) (denote-pattern* pat* penv senv)))
          (values penv
                  (lambda (env)
                    (let-values (((st0 vpred) ((dpred env) #t)))
                      (if (not st0) (error `(invalid-predicate ,predicate))
                        (let ((p* (d* env))
                              (pred
                                (lambda (st v)
                                  (let-values (((st result) ((vpred v) st)))
                                    (if (not st)
                                      (error `(predicate-failed ,predicate))
                                      (values st result))))))
                          (pattern
                            (lambda (st penv v)
                              (let-values (((st result) (pred st v)))
                                (if result
                                  ; TODO: var?
                                  ((pattern-sat p*) st penv v)
                                  (values #f #f))))
                            (lambda (st penv v)
                              (let-values (((st result) (pred st v)))
                                (if result
                                  ; TODO: var?
                                  ((pattern-nsat p*) st penv v)
                                  (values penv '()))))
                            ; TODO:
                            (lambda (st penv v) (values st penv))
                            ; TODO:
                            (lambda (st penv v) (values st penv)))))))))))
    ('_ (values penv denote-pattern-succeed))
    ((? symbol? vname) (denote-pattern-var penv vname penv))
    ((? quotable? datum) (denote-pattern-literal datum penv))))

(defrec match-chain scrutinee clauses)

; TODO: factor out loops?

(define (denote-match pt*-all vt senv)
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
                       (cons (cons dpat (cons drhs drhspat))
                             pc*))))))))
    (lambda (env)
      (let ((gv (dv env)))
        (lambda (st)
          (let*/state (((st v) (gv st)))
            (if (match-chain? v)
              (values st (match-chain
                           (match-chain-scrutinee v)
                           (cons (cons env pc*) (match-chain-clauses v))))
              (let loop ((pc* pc*))
                (if (null? pc*) (values #f #f)
                  (let* ((dpat (caar pc*))
                         (drhs (cadar pc*))
                         (drhspat (cddar pc*))
                         (pat (dpat env)))
                    (let-values
                      (((penv svs) ((pattern-sat pat) st '() v))
                       ((commit)
                        (lambda ()
                          (let*/state
                            (((st penv) ((pattern-assert pat) st '() v)))
                            ((drhs (append penv env)) st)))))
                      (if svs
                        (if (null? svs)
                          ((drhs (append penv env)) st)
                          (let-values
                            (((nst penv) ((pattern-nassert pat) st '() v)))
                            (if nst
                              (let ambiguous ((pc*1 (cdr pc*)))
                                (if (null? pc*1)
                                  (commit)
                                  (let-values
                                    (((penv1 svs1)
                                      ((pattern-sat ((caar pc*1) env))
                                       nst '() v)))
                                    (if svs1
                                      (values
                                        st (match-chain
                                             (append svs1 svs)
                                             v (list (cons env (cons (car pc*)
                                                                     pc*1)))))
                                      (ambiguous (cdr pc*1))))))
                              (commit))))
                        (loop (cdr pc*))))))))))))))


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
           (denote-match `((#f ,alt-false) (_ ,alt-true)) condition senv))
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
         (`(match ,scrutinee . ,pt*) (denote-match pt* scrutinee senv)))))))

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
      (_ #f))))

(define (eval-denoted-term dterm env) (dterm (map cddr env)))
(define (eval-term term env) (eval-denoted-term (denote-term term env) env))

;; 'dk-term' must be a valid dKanren program, *not* just any miniKanren term.
;; 'result' is a miniKanren term.
(define (dk-evalo dk-term result)
  (let ((dterm (denote-term dk-term initial-env)))
    (lambda (st)
      ; TODO: check for, and handle, match-chain vals
      (let-values (((st val) ((eval-denoted-term dterm initial-env) st)))
        (and st (unify st result val))))))

(define (primitive params body)
  (let-values (((st v) (((denote-lambda params body '()) '()) #t)))
    (if (not st) (error `(invalid-primitive (lambda ,params ,body)))
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

  ;(test "test 24"
    ;(run* (q) (== 5 q) (absento 5 q))
    ;'())
  ;(test "test 25"
    ;(run* (q) (== q `(5 6)) (absento 5 q))
    ;'())
  ;(test "test 25b"
    ;(run* (q) (absento 5 q) (== q `(5 6)))
    ;'())
  ;(test "test 26"
    ;(run* (q) (absento 5 q) (== 5 q))
    ;'())
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
  ;(test "test 66"
    ;(run* (q) (absento 6 5))
    ;'((_.0)))

  ;(test "absento 'closure-1a"
    ;(run* (q) (absento 'closure q) (== q 'closure))
    ;'())
  ;(test "absento 'closure-1b"
    ;(run* (q) (== q 'closure) (absento 'closure q))
    ;'())
  ;(test "absento 'closure-2a"
    ;(run* (q) (fresh (a d) (== q 'closure) (absento 'closure q)))
    ;'())
  ;(test "absento 'closure-2b"
    ;(run* (q) (fresh (a d) (absento 'closure q) (== q 'closure)))
    ;'())
  ;(test "absento 'closure-4a"
    ;(run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure a)))
    ;'())
  ;(test "absento 'closure-4b"
    ;(run* (q) (fresh (a d) (absento 'closure q) (== 'closure a) (== `(,a . ,d) q)))
    ;'())
  ;(test "absento 'closure-4c"
    ;(run* (q) (fresh (a d) (== 'closure a) (absento 'closure q) (== `(,a . ,d) q)))
    ;'())
  ;(test "absento 'closure-4d"
    ;(run* (q) (fresh (a d) (== 'closure a) (== `(,a . ,d) q) (absento 'closure q)))
    ;'())
  ;(test "absento 'closure-5a"
    ;(run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure d)))
    ;'())
  ;(test "absento 'closure-5b"
    ;(run* (q) (fresh (a d) (absento 'closure q) (== 'closure d) (== `(,a . ,d) q)))
    ;'())
  ;(test "absento 'closure-5c"
    ;(run* (q) (fresh (a d) (== 'closure d) (absento 'closure q) (== `(,a . ,d) q)))
    ;'())
  ;(test "absento 'closure-5d"
    ;(run* (q) (fresh (a d) (== 'closure d) (== `(,a . ,d) q) (absento 'closure q)))
    ;'())
  ;(test "absento 'closure-6"
    ;(run* (q)
      ;(== `(3 (closure x (x x) ((y . 7))) #t) q)
      ;(absento 'closure q))
    ;'())
  )

(module+ test
  (define-syntax test-eval
    (syntax-rules ()
     ((_ tm result)
      (let ((tm0 tm))
        (check-true (term? tm0 initial-env))
        (let-values (((st v) ((eval-term tm0 initial-env) #t)))
          (check-equal? v result))))))
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
  (test-eval '(and #f 9 10) #f)
  (test-eval '(and 8 9 10) 10)
  (test-eval '(or #f 11 12) 11)
  (test-eval '(let ((p (cons 8 9))) (cdr p)) 9)

  (define ex-append
    '(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       (list (append '() '()) (append '(foo) '(bar)) (append '(1 2) '(3 4)))) )
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

  (define ex-eval-expr
    '(letrec
       ((eval-expr
          (lambda (expr env)
            (match expr
              (`(quote ,datum) datum)
              (`(lambda (,(? symbol? x)) ,body)
                (lambda (a)
                  (eval-expr body (lambda (y)
                                    (if (equal? y x) a (env y))))))
              ((? symbol? x) (env x))
              (`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env)))
              (`(,rator ,rand) ((eval-expr rator env)
                                (eval-expr rand env)))))))
       (list
         (eval-expr '((lambda (y) y) 'g1) 'initial-env)
         (eval-expr '(((lambda (z) z) (lambda (v) v)) 'g2) 'initial-env)
         (eval-expr '(((lambda (a) (a a)) (lambda (b) b)) 'g3) 'initial-env)
         (eval-expr '(((lambda (c) (lambda (d) c)) 'g4) 'g5) 'initial-env)
         (eval-expr '(((lambda (f) (lambda (v1) (f (f v1)))) (lambda (e) e)) 'g6) 'initial-env)
         (eval-expr '((lambda (g) ((g g) g)) (lambda (i) (lambda (j) 'g7))) 'initial-env))))
  (test-eval ex-eval-expr '(g1 g2 g3 g4 g6 g7))

  (define ex-eval-expr-dneg
    '(letrec
       ((eval-expr
          (lambda (expr env)
            (match expr
              (`(,(not (not 'quote)) ,datum) datum)
              (`(lambda (,(? symbol? x)) ,body)
                (lambda (a)
                  (eval-expr body (lambda (y)
                                    (if (equal? y x) a (env y))))))
              ((symbol x) (env x))
              (`(cons ,e1 ,e2) (cons (eval-expr e1 env) (eval-expr e2 env)))
              (`(,rator ,rand) ((eval-expr rator env)
                                (eval-expr rand env)))))))
       (list
         (eval-expr '((lambda (y) y) 'g1) 'initial-env)
         (eval-expr '(((lambda (z) z) (lambda (v) v)) 'g2) 'initial-env)
         (eval-expr '(((lambda (a) (a a)) (lambda (b) b)) 'g3) 'initial-env)
         (eval-expr '(((lambda (c) (lambda (d) c)) 'g4) 'g5) 'initial-env)
         (eval-expr '(((lambda (f) (lambda (v1) (f (f v1)))) (lambda (e) e)) 'g6) 'initial-env)
         (eval-expr '((lambda (g) ((g g) g)) (lambda (i) (lambda (j) 'g7))) 'initial-env))))
  (test-eval ex-eval-expr-dneg '(g1 g2 g3 g4 g6 g7))

  ; the goal is to support something like this interpreter
  (define ex-eval-complex
    `(let ((closure-tag ',(gensym "#%closure"))
           (prim-tag ',(gensym "#%primitive"))
           (empty-env '()))
       (let ((initial-env
               `((cons . (val . (,prim-tag . cons)))
                 (car . (val . (,prim-tag . car)))
                 (cdr . (val . (,prim-tag . cdr)))
                 (null? . (val . (,prim-tag . null?)))
                 (pair? . (val . (,prim-tag . pair?)))
                 (symbol? . (val . (,prim-tag . symbol?)))
                 (not . (val . (,prim-tag . not)))
                 (equal? . (val . (,prim-tag . equal?)))
                 (list . (val . (,closure-tag (lambda x x) ,empty-env)))
                 . ,empty-env))
             (closure-tag? (lambda (v) (equal? v closure-tag)))
             (prim-tag? (lambda (v) (equal? v prim-tag))))
         (letrec
           ((applicable-tag? (lambda (v) (or (closure-tag? v) (prim-tag? v))))
            (quotable? (lambda (v)
                         (match v
                           ((? symbol?) (not (applicable-tag? v)))
                           (`(,a . ,d) (and (quotable? a) (quotable? d)))
                           (_ #t))))
            (not-in-params? (lambda (ps sym)
                              (match ps
                                ('() #t)
                                (`(,a . ,d)
                                  (and (not (equal? a sym))
                                       (not-in-params? d sym))))))
            (param-list? (lambda (x)
                           (match x
                             ('() #t)
                             (`(,(? symbol? a) . ,d)
                               (and (param-list? d) (not-in-params? d a)))
                             (_ #f))))
            (params? (lambda (x)
                       (match x
                         ((? param-list?) #t)
                         (x (symbol? x)))))
            (in-env? (lambda (env sym)
                       (match env
                         ('() #f)
                         (`((,a . ,_). ,d)
                           (or (equal? a sym) (in-env? d sym))))))
            (extend-env*
              (lambda (params args env)
                (match `(,params . ,args)
                  (`(() . ()) env)
                  (`((,x . ,dx*) . (,a . ,da*))
                    (extend-env* dx* da* `((,x . (val . ,a)) . ,env))))))
            (lookup
              (lambda (env sym)
                (match env
                  (`((,y . ,b) . ,rest)
                    (if (equal? sym y)
                      (match b
                        (`(val . ,v) v)
                        (`(rec . ,lam-expr) `(,closure-tag ,lam-expr ,env)))
                      (lookup rest sym))))))
            (term?
              (lambda (term env)
                (letrec
                  ((term1? (lambda (v) (term? v env)))
                   (terms? (lambda (ts env)
                             (match ts
                               ('() #t)
                               (`(,t . ,ts)
                                 (and (term? t env) (terms? ts env)))))))
                  (match term
                    (#t #t)
                    (#f #t)
                    ((? number?) #t)
                    ((and (? symbol? sym)) (in-env? env sym))
                    (`(,(? term1?) . ,rands) (terms? rands env))
                    (`(quote ,datum) (quotable? datum))
                    (`(if ,c ,t ,f) (and (term1? c) (term1? t) (term1? f)))
                    (`(lambda ,params ,body)
                      (and (params? params)
                           (let ((res
                                   (match params
                                     ((and (not (? symbol?)) params)
                                      (extend-env* params params env))
                                     (sym `((,sym . (val . ,sym)) . ,env)))))
                             (term? body res))))
                    (`(letrec
                        ((,p-name ,(and `(lambda ,params ,body) lam-expr)))
                        ,letrec-body)
                      (and (params? params)
                           (let ((res `((,p-name
                                          . (rec . (lambda ,params ,body)))
                                        . ,env)))
                             (and (term? lam-expr res)
                                  (term? letrec-body res)))))
                    (_ #f)))))
            (eval-prim
              (lambda (prim-id args)
                (match `(,prim-id . ,args)
                  (`(cons ,a ,d) `(,a . ,d))
                  (`(car (,(and (not (? applicable-tag?)) a) . ,d)) a)
                  (`(cdr (,(and (not (? applicable-tag?)) a) . ,d)) d)
                  (`(null? ,v) (match v ('() #t) (_ #f)))
                  (`(pair? ,v) (match v
                                 (`(,(not (? applicable-tag?)) . ,_) #t)
                                 (_ #f)))
                  (`(symbol? ,v) (symbol? v))
                  (`(number? ,v) (number? v))
                  (`(not ,v) (match v (#f #t) (_ #f)))
                  (`(equal? ,v1 ,v2) (equal? v1 v2)))))
            (eval-term-list
              (lambda (terms env)
                (match terms
                  ('() '())
                  (`(,term . ,terms)
                    `(,(eval-term term env) . ,(eval-term-list terms env))))))
            (eval-term
              (lambda (term env)
                (let ((bound? (lambda (sym) (in-env? env sym)))
                      (term1? (lambda (v) (term? v env))))
                  (match term
                    (#t #t)
                    (#f #f)
                    ((? number? num) num)
                    (`(,(and 'quote (not (? bound?))) ,(? quotable? datum))
                      datum)
                    ((? symbol? sym) (lookup env sym))
                    ((and `(,op . ,_) operation)
                     (match operation
                       (`(,(or (? bound?) (not (? symbol?)))
                           . ,rands)
                         (let ((op (eval-term op env))
                               (a* (eval-term-list rands env)))
                           (match op
                             (`(,(? prim-tag?) . ,prim-id)
                               (eval-prim prim-id a*))
                             (`(,(? closure-tag?) (lambda ,x ,body) ,env^)
                               (let ((res (match x
                                            ((and (not (? symbol?)) params)
                                             (extend-env* params a* env^))
                                            (sym `((,sym . (val . ,a*))
                                                   . ,env^)))))
                                 (eval-term body res))))))
                       (`(if ,condition ,alt-true ,alt-false)
                         (if (eval-term condition env)
                           (eval-term alt-true env)
                           (eval-term alt-false env)))
                       (`(lambda ,params ,body)
                        `(,closure-tag (lambda ,params ,body) ,env))
                       (`(letrec ((,p-name (lambda ,params ,body)))
                           ,letrec-body)
                        (eval-term
                          letrec-body
                          `((,p-name . (rec . (lambda ,params ,body)))
                            . ,env))))))))))

        (let ((program ',ex-append))
          (and (term? program initial-env)
               (eval-term program initial-env)))))))
  (test-eval ex-eval-complex ex-append-answer)
  )
