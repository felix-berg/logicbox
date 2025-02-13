package boxprover

import scala.sys.Prop

trait Verifier[F, R <: Rule.WithTypes[F, V], V] {
  def verify(proof: Proof[F, R]): List[(ProofStep[F, R], V)]
}

case class PLVerifier() extends Verifier[PLFormula, PropLogicRule, PropLogicViolation] {
  type Rle = PropLogicRule
  type Form = PLFormula
  type Viol = PropLogicViolation
  type Step = ProofStep[Form, Rle]

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
    def line(f: String, rule: PropLogicRule, refs: List[ProofStep[PLFormula, PropLogicRule]]): ProofLine[PLFormula, PropLogicRule] =
      ProofLine(PLParser()(PLLexer()(f)), rule, refs)
  }
}
