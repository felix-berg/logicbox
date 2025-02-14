package boxprover

sealed trait ProofStep[+F, +R]
case class ProofLine[+F, +R](formula: F, rule: R, refs: List[ProofStep[F, R]]) extends ProofStep[F, R]
case class ProofBox[+F, +R, +I](info: I, proof: Proof[F, R]) extends ProofStep[F, R]

type Proof[+F, +R] = List[ProofStep[F, R]]
