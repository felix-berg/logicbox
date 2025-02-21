package logicbox.marshal

import logicbox.framework.{Reference}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.ModifiableProof._
import logicbox.framework.Proof
import logicbox.framework.ModifiableProof

import spray.json._

class SimpleProofJsonWriterTest extends AnyFunSpec {
  import logicbox.proof.ProofStubs._

  private type W = JsonWriter[Proof[StubFormula, StubRule, ?, Id]]
  describe("SimpleProofJsonWriter::write") {
    it("should write single node using delegates that simply return values") {
      val writer: W = SimpleProofJsonWriter(
        JsonWriter.func2Writer(_ => JsString("id")),
        JsonWriter.func2Writer(_ => JsString("formula")),
        JsonWriter.func2Writer(_ => JsString("justification")),
      )

      val proof = StubProof(
        rootSteps = Seq("step"),
        map = Map("step" -> StubLine())
      )

      val exp = JsObject(
        "uuid" -> JsString("id"),
        "stepType" -> JsString("line"),
        "formula" -> JsString("formula"),
        "justification" -> JsString("justification"),
      )

      writer.write(proof) shouldBe exp
    }
  }
}
