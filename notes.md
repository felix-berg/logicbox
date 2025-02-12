#### Framework of proof checking
- a proof is a sequence of steps
- a step is either
  - a 'line' is
    - a formula
    - a rule (testable)
    - references to other steps
  - a 'box' is
    - possible information relevant to steps referring to it (eg. fresh variables)
    - a list of steps

A formula has
- well-defined `==` (structural)
  - goal is that parsed formulas, which are structurally equal are equal (eg
      `parse("p -> q and r") == parse("(p -> (q and (r)))")`).
- is parsable from strings
- is printable (both to LaTeX and ASCII)

Each rule is *checkable*, meaning
- given a resulting formula
- given a list of references (steps)
- we can validate and report potential errors (potentially referring to given refs, formula)

A proof is *verifiable*, meaning that
- each step only refers to steps above it (by reference)
- each rule is checkable
