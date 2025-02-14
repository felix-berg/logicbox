package logicbox

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors
import netscape.javascript.JSObject

class IdableProofJsonFormatTest extends AnyFunSpec {
  import logicbox.framework._
  import spray.json._

  case class StubFormula(asASCII: String, asLaTeX: String)
  case class StubRule(name: String)

  def stubLine(id: String, f: StubFormula, r: StubRule, refids: List[String]): IdableProof.Line[StubFormula, StubRule] =
    IdableProofImpl.Line(id, f, r, refids.map{ refid => IdableProofImpl.Line(refid, StubFormula(s"${refid}_formula", s"${refid}_latexFormula"), StubRule(s"${refid}_rule_name"), Nil) })

  describe("IdableProofStepJsonFormat") {
    val format: JsonFormat[IdableProof.Step[StubFormula, StubRule]] = 
      IdableProofStepJsonFormat(rule => rule.name, form => form.asASCII, form => form.asLaTeX)

    it("should marshal line with no refs correctly, LaTeX should be escaped") {
      val id = "someid"
      val formulaASCII = "e = mc^2"
      val formulaLaTeX = "\\text{latex babiiee \"\"e} \\begin{||} %%&&"
      val ruleName = "farts"

      val line: IdableProof.Step[StubFormula, StubRule] = 
        IdableProofImpl.Line(id, StubFormula(formulaASCII, formulaLaTeX), StubRule(ruleName), Nil)

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

      val line = stubLine(id, StubFormula(formulaASCII, formulaLaTeX), StubRule(ruleName), refs)

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
  }
}
