#lang racket/base
(provide
  term?
  eval-term
  initial-env
  )

(require
  racket/list
  racket/match
  )

(module+ test
  (require
    rackunit
    ))

; match pattern compiler
; patterns: _, var, literal, ', `, ?, not, and, or
; ideally, minimize let-bound/fresh vars
; compile pattern for input v:
; _, var: just use (or not use) v
; 'literal or literal: (equal? v literal)
; `: enter quasiquote state
; quasiquoted (P . Q): (and (pair? v) let Pv = recurse P/(car v), let Qv = recurse Q/(cdr v))
; ? p P: (and (p v) (recurse P))
; and: conj*
; or: disj*
; not:
;   (not (not pat)) = pat
;   (not (and P Q)) = (or (not P) (not Q))
;   (not (or P Q)) = (and (not P) (not Q))
;
; implicit pattern negations carried forward to subsequent cases
; translate the same output to both direct and goal interpretations
;   direct can rearrange disjunctions to group algebraic tree nodes for fewer comparisons
;   goals can't easily do this without messing up scheduling priorities
;
; match/r
;
; optional match result-domain annotations
;   type or finite domain: support fast, conservative domain constraints
;     domain = _ (anything), ([quasi]quoted) literal, infinite set (i.e. number, symbol), list (union) of domains
;   last-of-this-value markers: if you have this value, it's the last case where it's possible, so commit to it

; TODO:
; basic mk variant
;   primitive goals for unify/cxs
;   goal scheduling
;   simple reify for debugging
;   bind, mplus w/ costs
;     dfs, ws, quota/cost?
;     cost must be able to express size-incrementing search
;       goal scoped or leaky costs?
;       quota wrapper?
;   simple take, run, for debugging
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
;     goal suspension
;       incomplete goals (due to nondeterminism) bind themselves to blocking logic vars
;         match scrutinees are the blocking vars
;       unifying such a logic var w/ value resumes the goal, at least until the next blocker
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

(define store-empty (hasheq))
(define store-set hash-set)
(define (list-add-unique xs v) (if (memq v xs) xs (car v xs)))
(define (list-append-unique xs ys)
  (if (null? xs) ys
    (let ((zs (list-append-unique (cdr xs) ys))
          (x0 (car xs)))
      (if (memq x0 ys) zs (cons x0 zs)))))

(struct var (name) #:transparent)

(define domain-full '(pair symbol number () #f #t))
(define (domain-remove dmn type) (remove dmn type))
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

(struct vattr (domain =/=s goals-det goals-nondet) #:transparent)
(define (vattrs-get vs vr) (hash-ref vs vr vattr-empty))
(define vattrs-set hash-set)
(define vattr-empty (vattr domain-full '() '() '()))
(define (vattr-domain-set va dmn)
  (vattr dmn (vattr-=/=s va) (vattr-goals-det va) (vattr-goals-nondet va)))
(define (vattr-=/=s-clear va)
  (vattr (vattr-domain va) '() (vattr-goals-det va) (vattr-goals-nondet va)))
(define (vattr-=/=s-add va val)
  (if (or (var? val) (domain-has-val? (vattr-domain va) val))
    (vattr (vattr-domain va)
           (list-add-unique (vattr-=/=s va) val)
           (vattr-goals-det va)
           (vattr-goals-nondet va))
    va))
(define (vattr-=/=s-has? va val) (memq val (vattr-=/=s va)))
(define (vattr-suspend-det va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (cons goal (vattr-goals-det va))
         (vattr-goals-nondet va)))
(define (vattr-suspend-nondet va goal)
  (vattr (vattr-domain va)
         (vattr-=/=s va)
         (vattr-goals-det va)
         (cons goal (vattr-goals-nondet va))))
(define (vattr-overlap? va1 va2)
  (domain-overlap? (vattr-domain va1) (vattr-domain va2)))
(define (vattr-intersect va1 va2)
  (let ((di (domain-intersect (vattr-domain va1) (vattr-domain va2))))
    (and di (vattr di
                   (list-append-unique (vattr-=/=s va1) (vattr-=/=s va2))
                   (append (vattr-goals-det va1) (vattr-goals-det va2))
                   (append (vattr-goals-nondet va1)
                           (vattr-goals-nondet va2))))))

(struct goal-suspended (tag result blocker resume) #:transparent)

(struct schedule (det det-deferred nondet) #:transparent)
(define schedule-empty (schedule '() '() '()))
(define (schedule-activate sch det nondet)
  (schedule (cons det (schedule-det sch))
            (schedule-det-deferred sch)
            (cons nondet (schedule-nondet sch))))
(define (schedule-defer sch goal)
  (schedule (schedule-det sch)
            (cons goal (schedule-det-deferred sch))
            (schedule-nondet sch)))
(define (schedule-undefer sch goal)
  (schedule (append (schedule-det sch) (schedule-det-deferred sch))
            '()
            (schedule-nondet sch)))

(struct state (vs goals schedule) #:transparent)
(define state-empty (state store-empty store-empty schedule-empty))
(define (state-var-get st vr) (vattrs-get (state-vs st) vr))
(define (state-var-set st vr va)
  (state (vattrs-set (state-vs st) vr va)
         (state-goals st)
         (state-schedule st)))
(define (state-suspend st det? vr goal)
  (state-var-set
    st vr (let ((va (state-var-get st vr)))
            (if det?
              (vattr-suspend-det va goal)
              (vattr-suspend-nondet va goal)))))
(define (state-suspend* st var-dets var-nondets goal)
  (let* ((goal-ref (gensym))
         (goals (hash-set (state-goals st) goal-ref goal))
         (vs (state-vs st))
         (vs (foldl (lambda (vr vs)
                      (vattrs-set vs vr (vattr-suspend-det
                                          (vattrs-get vs vr) goal-ref)))
                    vs var-dets))
         (vs (foldl (lambda (vr vs)
                      (vattrs-set vs vr (vattr-suspend-nondet
                                          (vattrs-get vs vr) goal-ref)))
                    vs var-nondets)))
    (state vs goals (state-schedule st))))

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
                             (schedule-activate (state-schedule st)
                                                (vattr-goals-det va)
                                                (vattr-goals-nondet va)))))
              (disunify* st val vps)))))
     (else #f)))
(define (state-var-=/= st vr va val)
  (if (or (eq? '() val) (eq? #f val) (eq? #t val))
    (state-var-type-=/= st vr va val)
    (state-var-set st vr (vattr-=/=s-add va val))))
(define (state-var-==-var st vr1 va1 vr2 va2)
  (let ((va (vattr-intersect va1 va2)))
    (and va (let ((=/=s (vattr-=/=s va))
                  (va (vattr-=/=s-clear va))
                  (st (state-var-set st vr1 vr2)))
              (disunify* (state-var-set st vr2 va) vr2 =/=s)))))
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

(define (walk st tm)
  (if (var? tm)
    (let ((vs (state-vs st)))
      (let loop ((vr tm))
        (let ((va (vattrs-get vs vr)))
          (cond ((vattr? va) (values vr va))
                ((var? va) (loop va))
                (else (values va #f))))))
    (values tm #f)))
(define (not-occurs? st vr tm)
  (if (pair? tm) (let-values (((ht _) (walk st (car tm))))
                   (let ((st (not-occurs? st vr ht)))
                     (and st (let-values (((tt _) (walk st (cdr tm))))
                               (not-occurs? st vr tt)))))
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
           (let ((st (unify st (car v1) (car v2))))
             (and st (unify st (cdr v1) (cdr v2)))))
          (else #f))))
(define (disunify* st v1 vs)
  (if (null? vs) st (let ((st (disunify st v1 (car vs))))
                      (and st (disunify* st v1 (cdr vs))))))
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


(define (succeed st) st)
(define (fail st) #f)


(define (quotable? v)
  (match v
    (`(,a . ,d) (and (quotable? a) (quotable? d)))
    ((? procedure?) #f)
    (_ #t)))

(define (rev-append xs ys)
  (if (null? xs) ys (rev-append (cdr xs) (cons (car xs) ys))))

(define (denote-value val) (lambda (env) val))
(define (denote-lambda params body senv)
  (match params
    ((and (not (? symbol?)) params)
     (let ((body (denote-term body (extend-env* params params senv))))
       (lambda (env) (lambda args (body (rev-append args env))))))
    (sym
      (let ((body (denote-term body `((val . (,sym . ,sym)) . senv))))
        (lambda (env) (lambda args (body (cons args env))))))))
(define (denote-variable sym senv)
  (let loop ((idx 0) (senv senv))
    (match senv
      (`((val . (,y . ,b)) . ,rest)
        (if (equal? sym y)
          (begin
            (lambda (env)
            (list-ref env idx)))
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
                  (lambda (env) `(,(da env) . ,(dd env)))))
    ((? quotable? datum) (denote-value datum))))
(define (denote-and t* senv)
  (match t*
    ('() (denote-value #t))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let ((d0 (denote-term t senv)) (d* (denote-and t* senv)))
        (lambda (env) (and (d0 env) (d* env)))))))
(define (denote-or t* senv)
  (match t*
    ('() (denote-value #f))
    (`(,t) (denote-term t senv))
    (`(,t . ,t*)
      (let ((d0 (denote-term t senv)) (d* (denote-or t* senv)))
        (lambda (env) (or (d0 env) (d* env)))))))
(define (denote-application proc a* senv)
  (let ((dp (denote-term proc senv)) (da* (denote-term-list a* senv)))
    (lambda (env) (apply (dp env) (da* env)))))
(define (denote-term-list terms senv)
  (let ((d* (map (lambda (term) (denote-term term senv)) terms)))
    (lambda (env) (map (lambda (d0) (d0 env)) d*))))

(define (denote-pattern-identity env penv v) penv)
(define (denote-pattern-literal literal penv)
  (values penv (lambda (k) (lambda (env penv v)
                             (and (equal? literal v) (k env penv v))))))
(define (denote-pattern-var b* vname penv)
  (let loop ((idx 0) (b* b*))
    (match b*
      ('() (values `((,vname ,vname) . ,penv)
                   (lambda (k) (lambda (env penv v)
                                 (k env (cons v penv) v)))))
      (`((,name ,x) . ,b*)
        (if (eq? name vname)
          (values penv
                  (lambda (k)
                    (lambda (env penv v)
                      (and (equal? (list-ref penv idx) v) (k env penv v)))))
          (loop (+ idx 1) b*))))))
(define (denote-pattern-qq qqpattern penv senv)
  (match qqpattern
    (`(,'unquote ,pat) (denote-pattern pat penv senv))
    (`(,a . ,d)
      (let-values (((penv da) (denote-pattern-qq a penv senv)))
        (let-values (((penv dd) (denote-pattern-qq d penv senv)))
          (values penv
                  (lambda (k)
                    (let* ((kd (dd k))
                           (k (lambda (env penv v)
                                ((da (lambda (_env penv _v)
                                       (kd env penv (cdr v))))
                                 env penv (car v)))))
                      (lambda (env penv v)
                        (and (pair? v) (k env penv v)))))))))
    ((? quotable? datum) (denote-pattern-literal datum penv))))
(define (denote-pattern-fail k) (lambda (env penv v) #f))
(define (denote-pattern-or pattern* penv senv)
  (match pattern*
    ('() (values penv denote-pattern-fail))
    (`(,pattern . ,pattern*)
      (let-values (((penv0 d0) (denote-pattern pattern penv senv))
                   ((penv1 d*) (denote-pattern-or pattern* penv senv)))
        (values (and (equal? penv0 penv1) penv0)
                (lambda (k)
                  (let ((k0 (d0 k)) (k* (d* k)))
                    (lambda (env penv v)
                      (or (k0 env penv v) (k* env penv v))))))))))
(define (denote-pattern* pattern* penv senv)
  (match pattern*
    ('() (values penv (lambda (k) k)))
    (`(,pattern . ,pattern*)
      (let-values (((penv d0) (denote-pattern pattern penv senv)))
        (let-values (((penv d*) (denote-pattern* pattern* penv senv)))
          (values penv
                  (lambda (k)
                    (let* ((k* (d* k)))
                      (lambda (env penv v)
                        ((d0 (lambda (_env penv _v) (k* env penv v)))
                         env penv v))))))))))
(define (denote-pattern pattern penv senv)
  (match pattern
    (`(quote ,(? quotable? datum)) (denote-pattern-literal datum penv))
    (`(quasiquote ,qqpat) (denote-pattern-qq qqpat penv senv))
    (`(not . ,pat*)
      (let-values (((penv-not dp) (denote-pattern* pat* penv senv)))
        (values (and penv-not penv)
                (lambda (k)
                  (let ((k-not (dp k)))
                    (lambda (env penv v)
                      (and (not (k-not env penv v)) (k env penv v))))))))
    (`(and . ,pat*) (denote-pattern* pat* penv senv))
    (`(or . ,pat*) (denote-pattern-or pat* penv senv))
    (`(? ,predicate . ,pat*)
      (let ((dpred (denote-term predicate senv)))
        (let-values (((penv dp*) (denote-pattern* pat* penv senv)))
          (values penv
                  (lambda (k)
                    (let ((k* (dp* k)))
                      (lambda (env penv v)
                        (and ((dpred env) v)
                             (k* env penv v)))))))))
    ('_ (values penv (lambda (k) k)))
    ((? symbol? vname) (denote-pattern-var penv vname penv))
    ((? quotable? datum) (denote-pattern-literal datum penv))))
(define (denote-match pt*-all vt senv)
  (let ((dv (denote-term vt senv))
        (km (let loop ((pt* pt*-all))
              (match pt*
                ('() (lambda (env v)
                       (error (format "~s" `(match-failure ,v ,pt*-all)))))
                (`((,pat ,rhs) . ,pt*)
                  (let-values (((penv dpat) (denote-pattern pat '() senv)))
                    (let-values (((ps vs) (split-bindings penv)))
                      (let ((drhs (denote-term
                                    rhs (extend-env*
                                          (reverse ps) (reverse vs) senv)))
                            (k (dpat denote-pattern-identity))
                            (km (loop pt*)))
                        (lambda (env v)
                          (let ((penv (k env '() v)))
                            (if penv
                              (drhs (append penv env))
                              (km env v))))))))))))
    (lambda (env) (km env (dv env)))))

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
           (let ((dc (denote-term condition senv))
                 (dt (denote-term alt-true senv))
                 (df (denote-term alt-false senv)))
             (lambda (env) (if (dc env) (dt env) (df env)))))
         (`(lambda ,params ,body) (denote-lambda params body senv))
         (`(let ,binding* ,let-body)
           (let-values (((ps vs) (split-bindings binding*)))
             (let ((dp (denote-lambda ps let-body senv))
                   (da* (denote-term-list vs senv)))
               (lambda (env) (apply (dp env) (da* env))))))
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

(define empty-env '())
(define initial-env `((val . (cons . ,cons))
                      (val . (car . ,car))
                      (val . (cdr . ,cdr))
                      (val . (null? . ,null?))
                      (val . (pair? . ,pair?))
                      (val . (symbol? . ,symbol?))
                      (val . (number? . ,number?))
                      (val . (not . ,not))
                      (val . (equal? . ,equal?))
                      (val . (list . ,(lambda x x)))
                      . ,empty-env))

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

(define (pattern-var? b* vname ps)
  (match b*
    ('() `(,vname . ,ps))
    (`(,name . ,b*) (if (eq? name vname) ps (pattern-var? b* vname ps)))))
(define (pattern-qq? qqpattern ps env)
  (match qqpattern
    (`(,'unquote ,pat) (pattern? pat ps env))
    (`(,a . ,d) (let ((ps (pattern-qq? a ps env)))
                  (and ps (pattern-qq? d ps env))))
    ((? quotable?) ps)))
(define (pattern-or? pattern* ps env)
  (match pattern*
    ('() ps)
    (`(,pattern . ,pattern*)
      (let ((ps0 (pattern? pattern ps env)))
        (and (equal? ps0 (pattern-or? pattern* ps env)) ps0)))))
(define (pattern*? pattern* ps env)
  (match pattern*
    ('() ps)
    (`(,pattern . ,pattern*)
      (let ((ps (pattern? pattern ps env)))
        (and ps (pattern*? pattern* ps env))))))
(define (pattern? pattern ps env)
  (match pattern
    (`(quote ,(? quotable?)) ps)
    (`(quasiquote ,qqpat) (pattern-qq? qqpat ps env))
    (`(not . ,pat*) (pattern*? pat* ps env))
    (`(and . ,pat*) (pattern*? pat* ps env))
    (`(or . ,pat*) (pattern-or? pat* ps env))
    (`(? ,predicate . ,pat*)
      (and (term? predicate env) (pattern*? pat* ps env)))
    ('_ ps)
    ((? symbol? vname) (pattern-var? ps vname ps))
    ((? quotable?) ps)))
(define (match-clauses? pt* env)
  (match pt*
    ('() #t)
    (`((,pat ,rhs) . ,pt*)
      (let ((ps (pattern? pat '() env)))
        (and
          ps (term? rhs (extend-env* ps ps env)) (match-clauses? pt* env))))))

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

(define (eval-term term env) ((denote-term term env) (map cddr env)))

(module+ test
  (check-equal? (eval-term 3 initial-env) 3)
  (check-equal? (eval-term '3 initial-env) 3)
  (check-equal? (eval-term ''x initial-env) 'x)
  (check-equal? (eval-term ''(1 (2) 3) initial-env) '(1 (2) 3))
  (check-equal? (eval-term '(car '(1 (2) 3)) initial-env) 1)
  (check-equal? (eval-term '(cdr '(1 (2) 3)) initial-env) '((2) 3))
  (check-equal? (eval-term '(cons 'x 4) initial-env) '(x . 4))
  (check-equal? (eval-term '(null? '()) initial-env) #t)
  (check-equal? (eval-term '(null? '(0)) initial-env) #f)
  (check-equal? (eval-term '(list 5 6) initial-env) '(5 6))
  (check-equal? (eval-term '(and #f 9 10) initial-env) #f)
  (check-equal? (eval-term '(and 8 9 10) initial-env) 10)
  (check-equal? (eval-term '(or #f 11 12) initial-env) 11)
  (check-equal? (eval-term '(let ((p (cons 8 9))) (cdr p)) initial-env) 9)

  (define ex-append
    '(letrec ((append
                (lambda (xs ys)
                  (if (null? xs) ys (cons (car xs) (append (cdr xs) ys))))))
       (list (append '() '()) (append '(foo) '(bar)) (append '(1 2) '(3 4)))) )
  (define ex-append-answer '(() (foo bar) (1 2 3 4)))
  (check-true (term? ex-append initial-env))
  (check-equal? (eval-term ex-append initial-env) ex-append-answer)
  (check-equal? (eval-term '`(1 ,(car `(,(cdr '(b 2)) 3)) ,'a) initial-env)
    '(1 (2) a))

  (check-equal?
    (eval-term
      '(match '(1 (b 2))
         (`(1 (a ,x)) 3)
         (`(1 (b ,x)) x)
         (_ 4))
      initial-env)
    2)
  (check-equal?
    (eval-term
      '(match '(1 1 2)
         (`(,a ,b ,a) `(first ,a ,b))
         (`(,a ,a ,b) `(second ,a ,b))
         (_ 4))
      initial-env)
    '(second 1 2))

  (define ex-match
    '(match '(1 2 1)
       (`(,a ,b ,a) `(first ,a ,b))
       (`(,a ,a ,b) `(second ,a ,b))
       (_ 4)))
  (check-true (term? ex-match initial-env))
  (check-equal?
    (eval-term ex-match initial-env)
    '(first 1 2))

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
  (check-true (term? ex-eval-expr initial-env))
  (check-equal?
    (eval-term ex-eval-expr initial-env)
    '(g1 g2 g3 g4 g6 g7))

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
  (check-true (term? ex-eval-complex initial-env))
  (check-equal? (eval-term ex-eval-complex initial-env) ex-append-answer)
  )
