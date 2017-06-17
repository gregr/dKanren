;; Lattice-based constraint handling

;; Approximate orthogonality:
;; It should be possible to plug in new lattice components without
;; invalidating existing rules, but those rules may be incomplete on their own
;; with respect to new components (i.e., too conservative, not noticing some
;; new unsatisfiable situations).  Specifying additional "glue" rules to cover
;; new combinations should help reduce new sources of incompleteness.

;; Supported typed lattices:
;; * bottom: represents failure
;; * singleton: #t, #f, ()
;; * symbol:
;;   finite complement domain (fcd)
;;   > finite domain
;;   > singleton
;; * number:
;;   int? + closed intervals (infinity endpoints allowed) + fcd + arithmetic
;;   > singleton
;; * pair:
;;   car, cdr sub-lattices + finite complement shape domain + absents
;;   > car, cdr sub-lattices + finite shape domain
;;   > singleton + released finite [complement] shape constraints
;; * type-union: join of typed lattices
;; * near-top (unbound variable w/ optional =/= and absento constraints):
;;   fcd + absents

;; Supported constraints:
;; ==, =/=, typeo, integero, +o, *o, <=o

;; Finite control operators:
;; finite-and, finite-or, finite-xor, finite-not
;; Behind the scenes, these will meet, join, and complement the lattices
;; involved.  In some cases, this works well enough to eliminate the need for
;; search.  Even when search is necessary, approximate constraints can be given
;; and applied deterministically.  Since the purpose is to notice ASAP when
;; constraints become unsatisfiable, and not to provide a generative model,
;; search can be lazy, stopping at the first instance of satisfaction.  Ideally
;; we would use watched variables to trigger resumption of satisfiability
;; checking only on demand.

;; These constraints can be expressed with finite control operators:
;; (not-betweeno x a b): (finite-xor (<=o x a) (<=o b x))
;; (withino x a b):      (finite-and (<=o a x) (<=o x b))

;; Worries:
;; Is this going to end up gravitating towards being a general SMT solver?
