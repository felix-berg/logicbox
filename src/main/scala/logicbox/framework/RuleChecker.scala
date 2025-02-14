package logicbox.framework

trait RuleChecker[F, R, V] {
  import logicbox.framework.Proof.Step
  def check(rule: R, formula: F, refs: List[Step[F, R]]): List[V]
}
