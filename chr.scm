;; This approach is pretty complicated, and I'm not sure if there are enough
;; benefits to pursue it.  Although the declarative rule specification and
;; semantics are nice, term-rewriting systems require analyses to ensure
;; reasonable behavior (e.g., confluence).  This is probably because it's a
;; powerful computational medium on its own.  An ideal medium for constraint
;; handling shouldn't need powerful computation, and in exchange would provide
;; reasonable behavior for free.  I'm leaning towards semilattices for the
;; monotonicity guarantees.

;; TODO: an idemptotent CHR for simplification and constraint handling.
;; builtins: #f (failure), #t (trivial success), ground (non-variable), ==
;; auto-normalizing rearrangement rules (commutativity, associativity...)
;;   only rearrange when the output is "more normalized" than the input
;;   "more normalized" means lexicographically less-than
;;     w.r.t. ordering on data and variables (oldest first)
;; primitive operation rule interpretations
;; disjunctions?
;;   lazy satisfiability, optional path/clause learning to speed up revisits
;;   maybe limited to describing finite domains for simplicity
;; hooks for external solvers and search
;;   to support complex, possibly recursive, possibly non-terminating goals
;;   e.g., after simplifying `evalo` uses, expand definition in miniKanren
;; should be able to support these user-defined constraints:
;;   type predicates, =/=, absent, +, *, <=, integer
;;   possibly finite domains
;;   possibly more complex data structure predicates (sets?)

;; simpagation with optional host interpretation:
;; name @ static & input => output(with embedded host expression calculations)
;;
;; Input predicates are consumed when a rule fires, static predicates are not.
;; There's no need to separate builtins from user-defined predicates.
;; With embedded host calculation, can this even define some of the builtins?

;; rule prioritization:
;; * normalization: input and output only differ by argument rearrangement
;; * simplification: some inputs are consumed
;; * propagation: no inputs are consumed

;; A normalization rule only triggers if its output will be lexicographically
;; smaller than its input.
;;   e.g., (b+a=c => a+b=c) only triggers if (a lex< b).
;; Rule definitions themselves should be automatically normalized as a
;; preprocessing step.  For some rules, this normalization alone is enough to
;; match all normalized inputs.  When this isn't enough, rules will
;; automatically try all permutations of candidate input arrangements.
;;   e.g., (x+0=y => x=y) becomes (0+x=y -> x=y) due to (b+a=c -> a+b=c).
;;   (-1+0=q) would fail to match the normalized rule, but this rule knows to
;;   try permuting, giving (0+-1=q), which does match, yielding (-1=q).
;;     This example assumes literal values are considered lex< variables.
;; By normalizing both predicates and rules, matches should be easier to find.
;; Ultimately, adding rearrangement rules will never result in fewer matches,
;; and will not risk looping.

;; state = <new, accumulated, recent-history>
;; Instead of <G;S;B;T>, we have something more like <G;S+B;T>
;; Accumulated knowledge is idempotent.  This may mean that we need less
;; propagation history to avoid trivial nontermination.
;; Accumulated knowledge may have a fancy representation for efficiency.
;;   e.g., idempotent substitution and constraint store
;; Before moving onto the next, each new predicate is tested against all
;; relevant rule heads (in prioritized order), stopping and firing at the first
;; match.  Outputs from the matched rule may be processed before remaining new
;; predicates (LIFO).  In fact, it's possible that all new predicates will be
;; ordered arbitrarily, for efficiency (e.g., processing == first).
;; mechanism for detecting loops isn't available, a matched rule may be added
;; to recent-history to detect and reject later matches.

;; Incomplete example (several more rules should probably be added)
(rule (add-interpret1 a b c)
  (known a) (known b) & (+ a b c) => (= ,(,+ a b) c))
(rule (add-interpret2 a b c)
  (known a) (known c) & (+ a b c) => (= ,(,- c a) b))
(rule (add-commutativity a b c)
  & (+ b a c) => (+ a b c))  ;; & is optional when no statics are present.
(rule (add-reflexivity-right a b c d)
  (+ a b c) & (+ a b d) => (= c d))  ;; notice that normalization makes commutative permutation unnecessary in this case.
(rule (add-reflexivity-left a b c d)
  (+ a b d) & (+ a c d) => (= b c))
(rule (add-associativity a b c d e f)
  (known a) (known d) (+ a b c) & (+ c d e) => (+ a d f) (+ b f e))
(rule (add-identity a b)
  (+ a 0 b) => (= a b))  ;; preprocessing should normalize this rule.
(rule (add-double a b)
  (+ a a b) => (* 2 a b))

(rule (mul-interpret1 a b c)
  (known a) (known b) & (* a b c) => (= ,(,* a b) c))
(rule (mul-interpret2 a b c)
  (known a) (known c) (=/= 0 a) & (* a b c) => (= ,(,/ c a) c))
(rule (mul-commutativity a b c)
  (* b a c) => (* a b c))
(rule (mul-reflexivity a b c d)
  (* a b c) (* a b d) & => (= c d))
(rule (mul-associativity a b c d e f)
  (known a) (known d) (* a b c) & (* c d e) => (* a d f) (* b f e))
(rule (mul-identity a b)
  (* 1 a b) => (= a b))
(rule (mul-zero a b)
  (* 0 a b) => (= 0 b))
