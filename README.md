# dKanren

The purpose of dKanren is to support a model of programming that is as declarative as possible, and that can also be used for logical inference.  The current version provides logic programming with symbolic constraints using a first order functional language syntax.  Future versions will also provide access to lower level primitives and the scheduler for more direct control.  The constraint system is designed to be extensible, to allow plugging in domain-specific solvers.


## Constraint satisfaction

* symbolic constraints
* numeric constraints and solving
* foundation based on lattices for extensibility
* support for true negation and universal quantification


## Scheduling

* delayed goals and lazy constraints
* eager following of deterministic paths
* demand-based guess ordering


## Functional logic programming

* dk-evalo
* implicit pattern negation


## Caveat

This is still an early work in progress.  For now, you'll probably be more interested in miniKanren and Barliman:

https://github.com/michaelballantyne/faster-miniKanren

https://github.com/webyrd/Barliman
