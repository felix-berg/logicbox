package logicbox.framework

trait RuleChecker[Formula, Rule, BoxInfo, Viol] {
  def check(rule: Rule, formula: Formula, refs: List[Reference[Formula, BoxInfo]]): List[Viol]
}
