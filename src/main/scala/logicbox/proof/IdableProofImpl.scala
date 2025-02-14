package logicbox.proof
import logicbox.framework.{Proof, IdableProof}

object IdableProofImpl {
  case class Line[+F, +R](id: String, formula: F, rule: R, refs: List[IdableProof.Step[F, R]]) 
    extends IdableProof.Line[F, R]
  case class Box[F, R, I](id: String, info: I, proof: IdableProof[F, R]) 
    extends IdableProof.Box[F, R, I]
}
