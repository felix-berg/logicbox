package boxprover

trait Verifier[F, R <: Rule[F]] {
}

object PropLogicVerifier extends Verifier[PLFormula, PropLogicRule] {

}

class IdAbleProofLine[F](override val formula: F, override val rule: Rule[F], override val refs: List[ProofStep[F]]) 
  extends ProofLine[F](formula, rule, refs)
{
  
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    println("hello world")

  }
}
