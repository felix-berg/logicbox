package boxprover

trait ViolationTrait {
  def expl: String
}

trait Rule[F] {
  type RuleSet <: Rule[F]
  type Violation <: ViolationTrait
  def check(formula: F, refs: List[ProofStep[F, RuleSet]]): List[Violation]
}
