package logicbox.framework

trait RuleChecker[F, R, B, Viol] {
  type Violation = Viol
  def check(rule: R, formula: F, refs: List[Reference[F, B]]): List[Viol]
}
