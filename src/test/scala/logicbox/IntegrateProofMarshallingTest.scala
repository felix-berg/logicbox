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
import akka.http.scaladsl.settings.ParserSettings.ErrorLoggingVerbosity.Simple
import logicbox.framework.VerifierResult
import logicbox.proof.PropLogicViolation

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
    
    val proof = List(l1, l2, l3, l4, l5, b2, b4, l19, l20)

    def sideToInt(s: Side): Int = s match {
      case logicbox.proof.PropLogicRule.Side.Left => 1
      case logicbox.proof.PropLogicRule.Side.Right => 2
    }

    def ruleToName(rl: Rl): String = rl match {
      case Premise() => s"premise" 
      case Assumption() => s"assumption"
      case AndElim(s) => s"and_elim_${sideToInt(s)}"
      case AndIntro() => s"and_intro"
      case OrIntro(side) => s"or_into_${sideToInt(side)})"
      case OrElim() => s"or_elim"
      case ImplicationIntro() => s"implies_intro"
      case ImplicationElim() => s"implies_elim"
      case NotIntro() => s"not_intro"
      case NotElim() => s"not_elim"
      case ContradictionElim() => s"bot_elim"
      case NotNotElim() => s"not_not_elim"
      case ModusTollens() => s"modus_tollens"
      case NotNotIntro() => s"not_not_intro"
      case ProofByContradiction() => s"proof_by_contradiction"
      case LawOfExcludedMiddle() => s"law_of_excluded_middle"
      case Copy() => s"copy"
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
