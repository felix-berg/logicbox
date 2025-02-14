package boxprover

import boxprover.ViolationTrait

sealed trait ProofStep[+F, +R <: Rule[F]]

case class ProofLine[+F, +R <: Rule[F]](formula: F, rule: R, refs: List[ProofStep[F, R]]) extends ProofStep[F, R]

case class ProofBox[+F, +R <: Rule[F], +I](info: I, proof: Proof[F, R]) extends ProofStep[F, R]

type Proof[+F, +R <: Rule[F]] = List[ProofStep[F, R]]
