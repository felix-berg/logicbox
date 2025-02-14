package logicbox


import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

class SimpleVerifierTest extends AnyFunSpec {
  import logicbox.pl._
  import logicbox.framework._

  describe("SimpleVerifier") {
    it("should show integration of rulecheck") {
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

      def line(str: String, rule: R, refs: List[Proof.Step[F, R]]): ProofLineWithId[F, R] =
        ProofLineWithId(PLParser()(PLLexer()(str)), rule, refs, generateId())

      val checker: RuleChecker[F, R, V] = DelegatingRuleChecker()
      val verifier = SimpleVerifier(checker)
      val l1 = line("p -> t", Premise(), Nil)
      val l2 = line("r -> s", Premise(), Nil)

      val l3 = line("p and r", Assumption(), Nil)
      val l4 = line("p", AndElim(Side.Left), List(l3))
      val l5 = line("r", AndElim(Side.Right), List(l3)) // implication gives t not r
      val l6 = line("q", ImplicationElim(), List(l4, l1))
      val l7 = line("s", ImplicationElim(), List(l5, l2))
      val l8 = line("q and r", AndIntro(), List(l6, l7))

      val box = StubBox(info = (), proof = List(l3, l4, l5, l6, l7, l8))
      val l9 = line("p and r -> q and s", ImplicationIntro(), List(box))

      val proof = List(l1, l2, box, l9)
      val results = verifier.verify(proof)
      Inspectors.forAtLeast(1, results) {
        _ should matchPattern {
          case VerifierResult(line: ProofLineWithId[_, _], violation) =>
        }
      }
    }
  }
}
