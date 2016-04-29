# Clank - Theorem Prover (Paraconsistent Propositional Logics)

A paraconsistent logic is a logic where the consequence relation is not explosive, 
i. e. where the law {A, ¬A}⊨B does not hold. A very good introduction can be found 
on the [Stanford Encyclopedia of Philosophie](http://plato.stanford.edu/entries/logic-paraconsistent/)

An explosive consequence relation is acceptable for mathematics, 
but a nuisance in domains that have to operate on sets of possibly inconsistent 
premisses like medical assistance systems or applications in robotics; 
these are the domains where paraconsistent logics shine.

## Supported Logics

Clank is a prover for various systems of paraconsistent propositional logic and also vanilla propositional logic:

* PC: Vanilla propositional calculus
* K3: System Kleen
* L3: System Lukaszewic
* LP: System Priest
* RM: System Routly-Meye

## Grammar

The syntax of the propositional formulas:

    Formula = Var 	-- all aplphanumeric strings can be used as identifiers
            | a && b	-- conjunction
            | a || b    -- disjunction	
            | a -> b    -- implication
            | a <-> b   -- equivalence

## Usage

Clank has a command line interface.
Arguments are devided in *Mandatory Arguments* and *Options*;  
All mandatory arguments and at least one option have to be provided for a successful query. 

Mandatory arguments are:

* `-f, --formula`: One or more formulas of propositional logic.

* `-s, --semantics`: One of the following:
    * `pc`: propositional calculus
    * `k3`: system Kleene
    * `l3`: system Lucasiewicz
    * `lp`: system Priest
    * `rm`: system Routly-Meyer

Options are:

* `-c, --classification`: classification of the formula into theorem, satisfiable formulas, and contradictions.
* `-p, --property`: property assertion; whether a given formula / set of formulas is `sat`, `valid` or `unsat`.
* `-ms, --models`: the set of models of a given formula (with respect to a given semantics).
* `-nf, --normalform`: not yet implemented
* `--t, --entails`: whether the first formula is the conclusion the remaining formulas are the set of premisses.
* `-h, --help`: the help file (the help is also shown in case of an invalid query)

The order of arguments doesn't matter.
Several queries be issued at once.

Use `clank --help` to get a full overview of the available commands.
Here are some simple examples (all exmaples use classical propositional calculus):

* Get the models of `a ∧ b → a`:

    > clank -f 'a && b -> a' -s pc -ms
 
    > "[[(a,T),(b,T)],[(a,T),(b,F)],[(a,F),(b,T)],[(a,F),(b,F)]]"

* Classify `a ∧ b → b`:

    > clank -f 'p -> (p -> q) -> q' -s pc -c

    > sat

* Check whether `p → (p → q) → q` is valid:

    > clank -f 'p -> (p -> q) -> q' -s pc -p valid

    > true

* Check whether `q` implies `p → q` in classical:

    clank -f 'p -> q' 'q' -s pc -ms


