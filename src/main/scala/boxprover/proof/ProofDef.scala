package boxprover

import boxprover.ViolationTrait

sealed trait ProofStep[+F, +R <: Rule[F]]

class ProofLine[+F, +R <: Rule[F]](val formula: F, val rule: R, val refs: List[ProofStep[F, R]]) extends ProofStep[F, R]
object ProofLine {
  def unapply[F, R <: Rule[F]](line: ProofLine[F, R]): Option[(F, R, List[ProofStep[F, R]])] =
    Some(line.formula, line.rule, line.refs)
}

class ProofBox[+F, +R <: Rule[F], +I](val info: I, val proof: Proof[F, R]) extends ProofStep[F, R]
object ProofBox {
  def unapply[F, R <: Rule[F], I](box: ProofBox[F, R, I]): Option[(I, Proof[F, R])] = Some (box.info, box.proof)
}

type Proof[+F, +R <: Rule[F]] = List[ProofStep[F, R]]
