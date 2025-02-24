package logicbox.proof

import logicbox.framework.{ Proof, ModifiableProof }

case class ProofImpl[F, R, B, Id](
  createLine: (F, R, Seq[Id]) => Proof.Line[F, R, Id],
  createBox:     (B, Seq[Id]) => Proof.Box[B, Id],
  steps: Map[Id, Proof.Step[F, R, B, Id]],
  rootSteps: Seq[Id]
) extends ModifiableProof[F, R, B, Id] {

}
