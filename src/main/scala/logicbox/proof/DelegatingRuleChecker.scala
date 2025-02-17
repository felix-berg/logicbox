package logicbox

import logicbox.framework.{Proof, CheckableRule, RuleChecker}

// rule checker that simply delegates to each rules' own `check` method (given that R is checkable)
class DelegatingRuleChecker[F, R <: CheckableRule[F, R, V], V] extends RuleChecker[F, R, V] {
  override def check(rule: R, formula: F, refs: List[Proof.Step[F, R]]): List[V] = 
    rule.check(formula, refs)
}
