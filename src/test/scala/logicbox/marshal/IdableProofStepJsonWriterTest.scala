package logicbox.marshal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.IdableProof.Step

class IdableProofStepJsonWriterTest extends AnyFunSpec {
  import logicbox.framework._
  import spray.json._

  case class StubFormula(asASCII: String, asLaTeX: String)
  case class StubRule(name: String)

  private def ruleToName(rule: StubRule) = rule.name
  private def formulaToASCII(f: StubFormula) = f.asASCII
  private def formulaToLaTeX(f: StubFormula) = f.asLaTeX

  case class StubProofWriter(var writeResult: JsValue = JsNull) extends JsonWriter[IdableProof[StubFormula, StubRule]] {
    override def write(obj: IdableProof[StubFormula, StubRule]): JsValue = writeResult
  }

  case class StubLine(_id: String, f: StubFormula, r: StubRule, refids: List[String])
    extends IdableProof.Line[StubFormula, StubRule] 
  {
    override def id: String = _id
    override def formula: StubFormula = f
    override def rule: StubRule = r
    override def refs: List[Step[StubFormula, StubRule]] = refids.map { refid => StubLine(refid, StubFormula(s"${refid}_formula", s"${refid}_latexFormula"), StubRule(s"${refid}_rule_name"), Nil) }
  }

  private def stubBox(_id: String, _proof: IdableProof[StubFormula, StubRule]): IdableProof.Box[StubFormula, StubRule, _] =
    new IdableProof.Box[StubFormula, StubRule, Unit] {
      override def proof: IdableProof[StubFormula, StubRule] = _proof
      override def info: Unit = ()
      override def id: String = _id
     }

  val stubProofWriter = StubProofWriter()
  val format: JsonWriter[IdableProof.Step[StubFormula, StubRule]] = 
    IdableProofStepJsonWriter(ruleToName, formulaToASCII, formulaToLaTeX, () => stubProofWriter)

  describe("IdableProofStepJsonFormat::write") {
    it("should marshal line with no refs correctly, LaTeX should be escaped") {
      val id = "someid"
      val formulaASCII = "e = mc^2"
      val formulaLaTeX = "\\text{latex babiiee \"\"e} \\begin{||} %%&&"
      val ruleName = "(b)or(ing)_introduction"

      val line: IdableProof.Step[StubFormula, StubRule] = StubLine(id, StubFormula(formulaASCII, formulaLaTeX), StubRule(ruleName), Nil)

      format.write(line) shouldBe JsObject(
        "stepType" -> JsString("line"),
        "uuid" -> JsString(id),
        "formula" -> JsString(formulaASCII),
        "latexFormula" -> JsString(formulaLaTeX),
        "justification" -> JsObject(
          "rule" -> JsString(ruleName),
          "refs" -> JsArray(Nil)
        )
      )
    }

    it("should marshal line with refs correctly") {
      val id = "gawlejhalkjf"
      val formulaASCII = "p -> q or R maybe something exists idk"
      val formulaLaTeX = "p"
      val ruleName = "bland_elimination"
      val refs = List("jasldiv103hvi2ovh1, æææasdfljk1fæ21")

      val line = StubLine(id, StubFormula(formulaASCII, formulaLaTeX), StubRule(ruleName), refs)

      format.write(line) shouldBe JsObject(
        "stepType" -> JsString("line"),
        "uuid" -> JsString(id),
        "formula" -> JsString(formulaASCII),
        "latexFormula" -> JsString(formulaLaTeX),
        "justification" -> JsObject(
          "rule" -> JsString(ruleName),
          "refs" -> JsArray(refs.map(JsString.apply))
        )
      )
    }

    it("should marshal box with a proof correctly") {
      val id = "xob"

      val (l1id, l1ascii, l1latex, l1rule, l1refs) = ("i am line1", "l1ASCII", "l1latex", "l1rule", List("l1ref1", "l1ref2", "l1ref3"))
      val (l2id, l2ascii, l2latex, l2rule, l2refs) = ("i am line2", "l2ASCII", "l2latex", "l2rule", List("l2ref1", "l2ref2", "l2ref3"))

      val proof = List(
        StubLine(l1id, StubFormula(l1ascii, l1latex), StubRule(l1rule), l1refs),
        StubLine(l1id, StubFormula(l1ascii, l1latex), StubRule(l1rule), l1refs),
      )

      val preparedJson = JsArray(List(JsString("THIS IS A TEST")))
      stubProofWriter.writeResult = preparedJson

      val box = stubBox(id, proof)

      format.write(box) shouldBe JsObject(
        "stepType" -> JsString("box"),
        "uuid" -> JsString(id),
        "proof" -> preparedJson
      )
    }
  }
}
