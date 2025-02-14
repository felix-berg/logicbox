package logicbox

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal

import spray.json.{RootJsonFormat, JsonFormat}

import logicbox.framework.IdableProof
import logicbox.pl.PLFormula
import logicbox.pl.PropLogicRule
import spray.json.{JsValue, JsObject, JsArray, JsString, JsNumber, JsTrue, JsFalse, JsNull, DeserializationException}
import spray.json.SerializationException

case class IdableProofStepJsonFormat[F, R](
  ruleToName: R => String,
  formulaToASCII: F => String,
  formulaToLaTeX: F => String
) extends JsonFormat[IdableProof.Step[F, R]] 
{
  override def read(json: JsValue): IdableProof.Step[F, R] = ???

  private def writeRefs(refs: List[IdableProof.Step[F, R]]): JsValue = 
    JsArray.apply(refs.map(r => JsString(r.id)))

  override def write(step: IdableProof.Step[F, R]): JsValue = (step: @unchecked) match {
    case line: IdableProof.Line[F, R] @unchecked => JsObject(
      "stepType" -> JsString("line"),
      "uuid" -> JsString(line.id),
      "formula" -> JsString(formulaToASCII(line.formula)),
      "latexFormula" -> JsString(formulaToLaTeX(line.formula)),
      "justification" -> JsObject(Map(
        "rule" -> JsString(ruleToName(line.rule)),
        "refs" -> writeRefs(line.refs)
      ))
    )
    case _ => throw new SerializationException("Unknown step type")
  }
}

case class IdableProofJsonFormat(stepFormat: JsonFormat[IdableProof.Step[PLFormula, PropLogicRule]]) 
  extends RootJsonFormat[IdableProof[PLFormula, PropLogicRule]] 
{
  override def read(json: JsValue): IdableProof[PLFormula, PropLogicRule] = json match {
    case JsArray(elements) => elements.map(stepFormat.read).toList
    case _ => throw DeserializationException("Expected that proof is array")
  }

  override def write(proof: IdableProof[PLFormula, PropLogicRule]): JsValue = 
    JsArray.apply(proof.map(stepFormat.write))
}

object Main {
  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem(Behaviors.empty, "things")
    val executionContext = actorSystem.executionContext

    
  }
}
