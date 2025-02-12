package boxprover

sealed trait Mismatch {
  def expl: String
}

trait Rule[F] {
  def check(formula: F, refs: List[ProofStep[F]]): List[Mismatch]
}

case class WrongNumberOfReferences(exp: Int, actual: Int, expl: String = "") extends Mismatch
case class ReferenceShouldBeBox(ref: Int, expl: String = "") extends Mismatch
case class ReferenceShouldBeLine(ref: Int, expl: String = "") extends Mismatch
case class ReferenceDoesntMatchRule(actual: Int, expl: String = "") extends Mismatch
case class ReferencesMismatch(refs: List[Int], expl: String = "") extends Mismatch
case class FormulaDoesntMatchReferences(refs: List[Int], expl: String = "") extends Mismatch
case class FormulaDoesntMatchRule(expl: String = "") extends Mismatch
