package logicbox.proof

import logicbox.framework.{Reference}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.ModifiableProof._
import logicbox.framework.Proof

class ProofImplTest extends AnyFunSpec {
  import ProofStubs._

  type Pf = ProofImpl[StubFormula, StubRule, StubBoxInfo, Id]

  describe("ProofImplTest::addLine") {
    it("should add line to empty proof") {
      val proof: Pf = ProofImpl.empty
      proof.addLine("id", Pos(id = Root, Direction.Below))
      proof.getStep("id") should matchPattern {
        case Proof.Line(_, _, _) =>
      }
    }
  }
}
