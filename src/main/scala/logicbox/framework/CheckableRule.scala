package logicbox.framework

trait CheckableRule[F, Self <: CheckableRule[F, Self, V], V] {
  def check(formula: F, refs: List[Proof.Step[F, Self]]): List[V]
}
