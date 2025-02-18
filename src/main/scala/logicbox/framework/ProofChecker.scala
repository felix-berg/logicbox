package logicbox.framework

trait ProofChecker[F, R, B, Id, D] {
  type Diagnostic = D
  def check(proof: Proof[F, R, B, Id]): List[Diagnostic]
}
