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
      proof.addLine("id", ProofTop) match {
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

      proof = proof.addLine("1", ProofTop).getOrElse(???)
      proof = proof.addLine("2", AtLine("1", Direction.Below)).getOrElse(???)
 
      proof.getStep("2") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("1", "2")
    }

    it("should allow adding above existing line") {
      var proof: Pf = ProofImpl.empty

      proof = proof.addLine("1", ProofTop).getOrElse(???)
      proof = proof.addLine("2", AtLine("1", Direction.Above)).getOrElse(???)
 
      proof.getStep("2") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("2", "1")

      proof = proof.addLine("3", AtLine("1", Direction.Above)).getOrElse(???)
      proof.getStep("3") should matchPattern {
        case Right(Proof.Line(_, _, _)) =>
      }
      proof.rootSteps shouldBe Seq("2", "3", "1")
    }

    it("should allow adding line inside box by prepending to existing line") {
      var proof: Pf = ProofImpl.empty
      proof = proof.addBox("box", ProofTop).getOrElse(???)
      proof = proof.addLine("l1", BoxTop("box")).getOrElse(???)
      proof = proof.addLine("l2", AtLine("l1", Direction.Above)).getOrElse(???)

      proof.rootSteps shouldBe Seq("box")
      
      val Right(step) = proof.getStep("box"): @unchecked
      step.isInstanceOf[Proof.Box[?, ?]] shouldBe true
      val box = step.asInstanceOf[Proof.Box[B, Id]]

      box.steps shouldBe Seq("l2", "l1")
    }
  }

  describe("ProofImpl::addBox") {
    it("should allow adding empty box to proof") {
      var proof: Pf = ProofImpl.empty
      proof = proof.addBox("box", ProofTop).getOrElse(???)
      
      proof.getStep("box") should matchPattern {
        case Right(Proof.Box(_, _)) => 
      }
      proof.rootSteps shouldBe Seq("box")
    }

    it("should allow adding box above another  box") {
      var proof: Pf = ProofImpl.empty
      proof = proof.addBox("b1", ProofTop).getOrElse(???)
      
      proof.getStep("b1") should matchPattern {
        case Right(Proof.Box(_, _)) => 
      }
      proof.rootSteps shouldBe Seq("b1")

      proof = proof.addBox("b2", AtLine("b1", Direction.Above)).getOrElse(???)
      
      proof.getStep("b2") should matchPattern {
        case Right(Proof.Box(_, _)) => 
      }
      proof.rootSteps shouldBe Seq("b2", "b1")
    }

    it("should allow adding box as only element of box") {
      var proof: Pf = ProofImpl.empty
      proof = proof.addBox("b1", ProofTop).getOrElse(???)
      
      proof.getStep("b1") should matchPattern {
        case Right(Proof.Box(_, _)) => 
      }
      proof.rootSteps shouldBe Seq("b1")

      proof = proof.addBox("b2", BoxTop("b1")).getOrElse(???)
      
      proof.getStep("b2") should matchPattern {
        case Right(Proof.Box(_, _)) => 
      }
      proof.rootSteps shouldBe Seq("b1")
      
      val Right(step) = proof.getStep("b1"): @unchecked
      step.isInstanceOf[Proof.Box[?, ?]] shouldBe true
      val box = step.asInstanceOf[Proof.Box[B, Id]]
      box.steps shouldBe Seq("b2")
    }
  }
}
