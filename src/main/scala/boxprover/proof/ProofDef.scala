package boxprover

import boxprover.Violation

sealed trait ProofStep[+Formula]

class ProofLine[F](val formula: F, val rule: Rule[F], val refs: List[ProofStep[F]]) extends ProofStep[F]
object ProofLine {
  def unapply[F](line: ProofLine[F]): Option[(F, Rule[F], List[ProofStep[F]])] =
    Some(line.formula, line.rule, line.refs)
}

class ProofBox[F, +I](val info: I, val proof: Proof[F]) extends ProofStep[F]
object ProofBox {
  def unapply[F, I](box: ProofBox[F, I]): Option[(I, Proof[F])] = Some (box.info, box.proof)
}

type Proof[+F] = List[ProofStep[F]]
