package logicbox.proof
import logicbox.framework.{Proof, IdableProof, Idable}

object IdableProofImpl {
  case class Line[+F, +R](id: String, formula: F, rule: R, refs: List[Proof.Step[F, R] & Idable]) 
    extends Proof.Line[F, R] with Idable
  case class Box[F, R, I](id: String, info: I, proof: IdableProof[F, R]) 
    extends Proof.Box[F, R, I] with Idable
}
