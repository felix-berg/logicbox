package boxprover

trait ViolationTrait {
  def expl: String
}

abstract class Rule[F] {
  type RuleSet <: Rule[F]
  type Violation <: ViolationTrait
  def check(formula: F, refs: List[ProofStep[F, RuleSet]]): List[Violation]
}

object Rule {
  type WithTypes[F, V] = Rule[F] { type Violation = V }
}
