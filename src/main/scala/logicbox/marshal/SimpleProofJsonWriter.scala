package logicbox.marshal

import logicbox.framework.Proof
import spray.json._

case class SimpleProofJsonWriter[F, R, Id](
  idWriter: JsonWriter[Id], formulaWriter: JsonWriter[F],
  justficationWriter: JsonWriter[Justification[R, Id]]
) extends RootJsonWriter[Proof[F, R, ?, Id]] {

  override def write(proof: Proof[F, R, ?, Id]): JsValue = 
    val id = proof.rootSteps.headOption.getOrElse(???)
    val step = proof.getStep(id).getOrElse(???)
    val line = step.asInstanceOf[Proof.Line[F, R, Id]]
    JsObject(
      "stepType" -> JsString("line"),
      "uuid" -> idWriter.write(id),
      "formula" -> formulaWriter.write(line.formula),
      "justification" -> justficationWriter.write(Justification(line.rule, line.refs))
    )
}
