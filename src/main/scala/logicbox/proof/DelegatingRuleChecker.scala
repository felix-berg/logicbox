package logicbox

import logicbox.framework.{CheckableRule, RuleChecker, Reference}

// rule checker that simply delegates to each rules' own `check` method (given that R is checkable)
class DelegatingRuleChecker[F, R <: CheckableRule[F, I, V], I, V] extends RuleChecker[F, R, I, V] {
  override def check(rule: R, formula: F, refs: List[Reference[F, I]]): List[V] = 
    rule.check(formula, refs)
}
