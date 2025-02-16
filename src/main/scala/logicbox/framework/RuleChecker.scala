package logicbox.framework


trait RuleChecker[F, R, I, V] {
  def check(rule: R, formula: F, refs: List[Reference[F, I]]): List[V]
}
