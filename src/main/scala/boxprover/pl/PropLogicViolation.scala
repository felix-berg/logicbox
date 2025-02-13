package boxprover

sealed trait PropLogicViolation extends Violation

object PropLogicViolation {
  case class WrongNumberOfReferences(exp: Int, actual: Int, expl: String = "") extends PropLogicViolation
  case class ReferenceShouldBeBox(ref: Int, expl: String = "") extends PropLogicViolation
  case class ReferenceShouldBeLine(ref: Int, expl: String = "") extends PropLogicViolation
  case class ReferenceDoesntMatchRule(ref: Int, expl: String = "") extends PropLogicViolation
  case class ReferencesMismatch(refs: List[Int], expl: String = "") extends PropLogicViolation
  case class FormulaDoesntMatchReference(refs: Int, expl: String = "") extends PropLogicViolation
  case class FormulaDoesntMatchRule(expl: String = "") extends PropLogicViolation
  case class MiscellaneousMismatch(expl: String = "") extends PropLogicViolation
}
