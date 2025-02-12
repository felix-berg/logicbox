#### Framework of proof checking
- a **proof** is a sequence of steps
- a **step** is either
  - a 'line' is
    - a **formula**
    - a rule
    - references to other **steps**
  - a 'box' is
    - possible information relevant to steps referring to it (eg. fresh variables)
    - a list of steps

A **formula** has
- well-defined `==` (structural)
  - goal is that parsed formulas, which are structurally equal are equal (eg
      `parse("p -> q and r -> s") == parse("(p -> ((q and (r)) -> s))")`).
- is parsable from strings
- is printable (both to LaTeX and ASCII)

A **proof** is *verifiable*, meaning that
- each step only refers to steps above it (by reference)
- each rule is checkable

Each **rule** `r` is *checkable*, meaning there exists a function `check(r, formula, refs)`
- given a resulting formula `formula`
- given a list of references `refs` (steps)
- we can validate and report potential **diagnostics**

A **diagnostic** reports either
- there is a wrong number of references
- a mismatch between rule/(reference(s), idx(s))
  - eg. the rule requires reference of form $\phi \rightarrow \psi$, but reference is $\phi \land \psi$.
- a mismatch between references
- a mismatch between formula and (reference(s), idx(s))
