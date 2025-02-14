package logicbox

import logicbox.framework.{Proof, RuleChecker, VerifierResult, Verifier}
import Proof.{Step, Line, Box}
import logicbox.pl.*

case class SimpleVerifier[F, R, V, C <: RuleChecker[F, R, V]](checker: C) extends Verifier[F, R, V] {
  def verify(proof: Proof[F, R]): List[VerifierResult[F, R, V]] =
    proof.flatMap {
      case line @ Line(formula, rule, refs) => 
        checker.check(rule, formula, refs).map(violation => VerifierResult(line, violation))
      case Box(info, proof) => verify(proof)
    }
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    type F = PLFormula
    type R = PropLogicRule
    type V = PropLogicViolation

    import PropLogicRule.*

    val generateId: () => String = {
      var x: Int = 0
      () => {
        x = x + 1
        x.toString
      }
    }

    def line(str: String, rule: R, refs: List[Step[F, R]]): ProofLineWithId[F, R] =
      ProofLineWithId(PLParser()(PLLexer()(str)), rule, refs, generateId())

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

    val box = Proof.Box(info = (), proof = List(l3, l4, l5, l6, l7, l8))
    val l9 = line("p and r -> q and s", ImplicationIntro(), List(box))

    val proof = List(l1, l2, box, l9)
    verifier.verify(proof).collect {
      case VerifierResult(where: ProofLineWithId[_, _], violation) => 
        println(s"${where.id}: $violation")
    }
  }
}
