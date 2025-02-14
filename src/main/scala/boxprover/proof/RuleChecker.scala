package boxprover

trait ViolationTrait {
  def expl: String
}

trait RuleChecker[F, R, V <: ViolationTrait] {
  def check(rule: R, formula: F, refs: List[ProofStep[F, R]]): List[V]
}
