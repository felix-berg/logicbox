package boxprover

import java.util.UUID

class IdAbleProofLine[F, R <: Rule[F]](override val formula: F, override val rule: Rule[F], override val refs: List[ProofStep[F, R]]) 
  extends ProofLine[F, R](formula, rule, refs)
{
  private val uuid = UUID.randomUUID().toString

  def getId: String = uuid
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    println("hello world")

  }
}
