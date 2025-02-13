package boxprover

import boxprover.Violation

sealed trait ProofStep[+Formula]
case class ProofLine[F](formula: F, rule: Rule[F], refs: List[ProofStep[F]]) extends ProofStep[F]

case class ProofBox[F, +I](info: I, proof: Proof[F]) extends ProofStep[F]

type Proof[+F] = List[ProofStep[F]]
