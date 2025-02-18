package logicbox.proof

import logicbox.framework._
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import org.scalatest.funspec.AnyFunSpec
import logicbox.framework.Proof.Step

class ScopedProofCheckerTest extends AnyFunSpec {
  import ProofStubs._

  describe("ScopedProofCheckerTest::check") {
    val checker = ScopedProofChecker[F, R, B, Id]()
    it("should allow empty proof") {
      val proof = StubProof()
      checker.check(proof) should be (Nil)
    }

    it("should diallow references to lines after current line") {
      val proof = StubProof(
        steps = Seq("1", "2"),
        map = Map(
          "1" -> StubLine(refs = Seq("2")),
          "2" -> StubLine()
        )
      )

      Inspectors.forAtLeast(1, checker.check(proof)) {
        _ should matchPattern {
          case _ => 
        }
      }
    }
  }
}
