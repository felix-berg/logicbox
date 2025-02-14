package boxprover

class DelegatingRuleChecker[F, R <: CheckableRule[F, R, V], V <: ViolationTrait] extends RuleChecker[F, R, V] {
  override def check(rule: R, formula: F, refs: List[ProofStep[F, R]]): List[V] = rule.check(formula, refs)
}
