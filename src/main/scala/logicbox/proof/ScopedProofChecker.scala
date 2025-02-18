package logicbox.proof

import logicbox.framework.ProofChecker
import logicbox.framework.Proof

object ScopedProofChecker {
  sealed trait Diagnostic
}

class ScopedProofChecker[F, R, B, Id] 
  extends ProofChecker[F, R, B, Id, ScopedProofChecker.Diagnostic] 
{
  def checkImpl(proof: Proof[F, R, B, Id], steps: Seq[Id], scope: Set[Id]): List[Diagnostic] =
    steps match {
      case _ => Nil
    }
    
  override def check(proof: Proof[F, R, B, Id]): List[Diagnostic] = 
    checkImpl(proof, proof.steps, Set.empty)
}
