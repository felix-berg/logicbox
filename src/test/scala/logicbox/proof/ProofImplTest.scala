package logicbox.proof

import logicbox.framework.{Reference}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.ModifiableProof._
import logicbox.framework.Proof
import logicbox.framework.ModifiableProof

class ProofImplTest extends AnyFunSpec {
  import ProofStubs._

  type Pf = ModifiableProof[Option[StubFormula], Option[StubRule], Option[StubBoxInfo], Id]

  describe("ProofImplTest::addLine") {
    it("should add line to empty proof") {
      val proof: Pf = ProofImpl.empty
      proof.addLine("id", Top) match {
        case Right(proof1) => 
          proof1.getStep("id") should matchPattern {
            case Right(Proof.Line(_, _, _)) =>
          }
          proof1.rootSteps should contain("id")
        case _ => ???
      }
    }

    it("should allow adding below existing line") {
      var proof: Pf = ProofImpl.empty

      proof = proof.addLine("1", Top).getOrElse(???)
      proof = proof.addLine("2", IdPos("1", Direction.Below)).getOrElse(???)
 
      proof.getStep("2") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("1", "2")
    }

    it("should allow adding above existing line") {
      var proof: Pf = ProofImpl.empty

      proof = proof.addLine("1", Top).getOrElse(???)
      proof = proof.addLine("2", IdPos("1", Direction.Above)).getOrElse(???)
 
      proof.getStep("2") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("2", "1")

      proof = proof.addLine("3", IdPos("1", Direction.Above)).getOrElse(???)
      proof.getStep("3") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("2", "3", "1")
    }

    // it("should not allow adding line with existing id") {
    // }
  }
}
