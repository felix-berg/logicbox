package logicbox.framework

trait CheckableRule[F, I, V] {
  def check(formula: F, refs: List[Reference[F, I]]): List[V]
}
