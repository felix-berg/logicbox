package logicbox.framework

object ProofChecker {
  sealed trait Diagnostic[+Id, +V] {
    def stepId: Id
  }

  case class RuleViolation[Id, V](stepId: Id, violation: V) extends Diagnostic[Id, V]
  case class StepIdNotFound[Id](stepId: Id, expl: String) extends Diagnostic[Id, Nothing]

  case class ReferenceIdNotFound[Id](stepId: Id, whichRef: Int, refId: Id, expl: String) extends Diagnostic[Id, Nothing]
  case class MissingReference[Id](stepId: Id, whichRef: Int, refId: Id, expl: String) extends Diagnostic[Id, Nothing]
  case class MalformedReference[Id](stepId: Id, whichRef: Int, refId: Id, expl: String) extends Diagnostic[Id, Nothing]
}

trait ProofChecker[F, R, B, V, Id] {
  val ruleChecker: RuleChecker[F, R, B, V]
  type Diagnostic = ProofChecker.Diagnostic[Id, V]

  def check(proof: Proof[F, R, B, Id]): List[Diagnostic]
}
