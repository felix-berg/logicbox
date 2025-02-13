package boxprover

trait Violation {
  def expl: String
}

trait Rule[F] {
  type V <: Violation
  def check(formula: F, refs: List[ProofStep[F]]): List[V]
}
