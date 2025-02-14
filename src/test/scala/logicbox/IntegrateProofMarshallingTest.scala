package logicbox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import logicbox.framework.IdableProof

import logicbox.formula.PLFormula
import logicbox.proof.PropLogicRule
import logicbox.proof.IdableProofImpl
import logicbox.marshal.IdableProofJsonWriter
import logicbox.marshal.IdableProofStepJsonWriter
import org.scalatest.Inside

class IntegrateProofMarshallingTest extends AnyFlatSpec {
  behavior of "Integration of IdableProofImpl and IdeableProofJsonWriter"

  import spray.json._
  type Fm = PLFormula
  type Rl = PropLogicRule

  def parse(str: String): Fm = {
    import logicbox.formula.{PLLexer, PLParser}
    PLParser()(PLLexer()(str))
  }

  def line(id: String, str: String, rule: Rl, refs: List[IdableProof.Step[Fm, Rl]]): IdableProof.Line[Fm, Rl] = 
    IdableProofImpl.Line(id, parse(str), rule, refs)

  def box(id: String, proof: IdableProof[Fm, Rl]): IdableProof.Box[Fm, Rl, Unit] = 
    IdableProofImpl.Box(id, (), proof)

  it should "produce the correct json given an example" in {
    import PropLogicRule._

    val l1  = line("1", "(p -> q) -> r", Premise(), Nil)
    val l2  = line("2", "s -> not p", Premise(), Nil)
    val l3  = line("3", "t", Premise(), Nil)
    val l4  = line("4", "(not s and t) -> q", Premise(), Nil)
    val l5  = line("5", "p or not p", LawOfExcludedMiddle(), Nil)

    // b2 begin
    val l6  = line("6", "p", Assumption(), Nil)
    val l7  = line("7", "not not p", NotNotIntro(), List(l6))
    val l8  = line("8", "not s", ModusTollens(), List(l2, l7))
    val l9  = line("9", "not s and t", AndIntro(), List(l8, l3))
    val l10 = line("10", "q", ImplicationElim(), List(l9, l4))

    // b1 begin
    val l11 = line("11", "p", Assumption(), Nil)
    val l12 = line("12", "q", Copy(), List(l10))

    val b1 = box("b1", List(l11, l12))
    // b1 end

    val l13 = line("13", "p -> q", ImplicationIntro(), List(b1))
    val b2 = box("b2", List(l6, l7, l8, l9, l10, b1, l13))
    // b2 end

    // b4 end
    val l14 = line("14", "not p", Assumption(), Nil)
    // b3 begin
    val l15 = line("15", "p", Assumption(), Nil)
    val l16 = line("16", "false", NotElim(), List(l15, l14))
    val l17 = line("17", "q", ContradictionElim(), List(l16))
    val b3 = box("b3", List(l15, l16, l17))
    // b3 end

    val l18 = line("18", "p -> q", ImplicationIntro(), List(b3))
    val b4 = box("b4", List(l14, b3, l18))
    // b4 end

    val l19 = line("19", "p -> q", OrElim(), List(l5, b2, b4))
    val l20 = line("20", "r", ImplicationElim(), List(l19, l1))
    
    val proof = List(l1, l2, l3, l4, l5, b1, b4, l19, l20)

    def ruleToName(rl: Rl): String = rl match {
      case Premise() => s"Premise" 
      case Assumption() => s"Assumption"
      case AndElim(side) => s"AndElim($side)"
      case AndIntro() => s"AndIntro"
      case OrIntro(side) => s"OrIntro($side)"
      case OrElim() => s"OrElim"
      case ImplicationIntro() => s"ImplicationIntro"
      case ImplicationElim() => s"ImplicationElim"
      case NotIntro() => s"NotIntro"
      case NotElim() => s"NotElim"
      case ContradictionElim() => s"ContradictionElim"
      case NotNotElim() => s"NotNotElim"
      case ModusTollens() => s"ModusTollens"
      case NotNotIntro() => s"NotNotIntro"
      case ProofByContradiction() => s"ProofByContradiction"
      case LawOfExcludedMiddle() => s"LawOfExcludedMiddle"
      case Copy() => s"Copy"
    }
    
    lazy val writer: spray.json.JsonWriter[IdableProof[Fm, Rl]] =
      IdableProofJsonWriter(IdableProofStepJsonWriter(ruleToName, f => f.toString, StupidPLFormulaToLatex.formulaToLaTeX, () => writer))

    val result = writer.write(proof)
    result shouldBe a [JsArray]
    val values: List[JsValue] = result.asInstanceOf[JsArray]._1.toList
    Inspectors.forAll(values) { 
      case v: JsObject => true 
      case _ => false
    }

    // if you want to look: 
    // println(result.prettyPrint)
  }
}
