package logicbox.framework

case class VerifierResult[F, R, V](where: Proof.Step[F, R], violation: V)

trait Verifier[F, R, V] {
  def verify(proof: Proof[F, R]): List[VerifierResult[F, R, V]]
}
