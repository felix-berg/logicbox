package logicbox.marshal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.IdableProof
import spray.json._
import logicbox.framework.IdableProof.Step

class IdableProofJsonWriterTest extends AnyFunSpec {
  case class StubFormula()
  case class StubRule()

  def stubLine(_id: String): IdableProof.Line[StubFormula, StubRule] = new IdableProof.Line[StubFormula, StubRule] {
    override def id = _id
    override def formula = StubFormula()
    override def rule = StubRule()
    override def refs = Nil
  }

  def doNothing(): JsValue = JsNull
  case class StubProofStepFormat() 
    extends JsonWriter[IdableProof.Step[StubFormula, StubRule]] 
  {
    override def write(obj: Step[StubFormula, StubRule]): JsValue = JsString(obj.id)
  }

  val stubStepFormat = StubProofStepFormat()
  val format: JsonWriter[IdableProof[StubFormula, StubRule]] = IdableProofJsonWriter(stubStepFormat)

  describe("IdableProofJsonWriter::write") {
    it("should marshall empty proof") {
      format.write(Nil) shouldBe JsArray()
    }

    it("should marshall non-empty proof") {
      format.write((1 to 5).map(i => stubLine(i.toString)).toList) shouldBe JsArray(
        JsString("1"), 
        JsString("2"), 
        JsString("3"), 
        JsString("4"), 
        JsString("5"), 
      )
    }
  }
}
