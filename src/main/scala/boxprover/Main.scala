package boxprover

trait Verifier[F, R <: Rule[F]] {
}

object PropLogicVerifier extends Verifier[PLFormula, PropLogicRule] {

}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    println("hello world")
  }
}
