package boxprover

case class VerifierResult[F, R, V](where: ProofStep[F, R], violation: V)
trait Verifier[F, R, V] {
  def verify(proof: Proof[F, R]): List[VerifierResult[F, R, V]]
}

case class SimpleVerifier[F, R, V <: ViolationTrait, C <: RuleChecker[F, R, V]](checker: C) extends Verifier[F, R, V] {
  def verify(proof: Proof[F, R]): List[VerifierResult[F, R, V]] =
    proof.flatMap {
      case line @ ProofLine(formula, rule, refs) => 
        checker.check(rule, formula, refs).map(violation => VerifierResult(line, violation))
      case ProofBox(info, proof) => verify(proof)
    }
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    type F = PLFormula
    type R = PropLogicRule
    type V = PropLogicViolation

    import PropLogicRule._

    def line(str: String, rule: R, refs: List[ProofStep[F, R]]): ProofLine[F, R] =
      ProofLine(PLParser()(PLLexer()(str)), rule, refs)

    val checker: RuleChecker[F, R, V] = DelegatingRuleChecker[F, R, V]()
    val verifier = SimpleVerifier(checker)
    val l1 = line("p -> q", Premise(), Nil)
    val l2 = line("r -> s", Premise(), Nil)

    val l3 = line("p and r", Assumption(), Nil)
    val l4 = line("p", AndElim(Side.Left), List(l3))
    val l5 = line("r", AndElim(Side.Right), List(l3))
    val l6 = line("q", ImplicationElim(), List(l4, l1))
    val l7 = line("s", ImplicationElim(), List(l5, l2))
    val l8 = line("q and r", AndIntro(), List(l6, l7))

    val box = ProofBox(info = (), proof = List(l3, l4, l5, l6, l7, l8))
    val l9 = line("p and r -> q and s", ImplicationIntro(), List(box))

    val proof = List(l1, l2, box, l9)
    verifier.verify(proof).foreach {
      case VerifierResult(where, violation) => println(s"$where: $violation")
    }
  }
}
