package boxprover

import scala.sys.Prop

trait Verifier[F, R <: Rule[F]] {
  type Result
  def verify(proof: Proof[F, R]): Result
}

case class PLVerifier() extends Verifier[PLFormula, PropLogicRule] {
  type Rle = PropLogicRule
  type Form = PLFormula
  type Viol = PropLogicViolation
  type Step = ProofStep[Form, Rle]

  type Result = List[(Step, Viol)]

  override def verify(proof: Proof[Form, Rle]): List[(Step, Viol)] = {
    proof.flatMap { step => (step: @unchecked) match {
      case ProofLine(formula, rule: Rle, refs: List[Step] @unchecked) => 
        rule.check(formula, refs).map(vlt => (step, vlt))
      case ProofBox(_, proof: Proof[Form, Rle]@unchecked) => verify(proof)
    }}
  }
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    
  }
}
