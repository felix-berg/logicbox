package logicbox.marshal 

import spray.json._
import logicbox.framework.IdableProof

case class IdableProofStepJsonFormat[F, R](
  ruleToName: R => String,
  formulaToASCII: F => String,
  formulaToLaTeX: F => String,
  proofFormat: JsonFormat[IdableProof[F, R]]
) extends JsonFormat[IdableProof.Step[F, R]] 
{
  override def read(json: JsValue): IdableProof.Step[F, R] = ???

  private def writeRefs(refs: List[IdableProof.Step[F, R]]): JsValue = 
    JsArray.apply(refs.map(r => JsString(r.id)))

  override def write(step: IdableProof.Step[F, R]): JsValue = step match {
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

    case box: IdableProof.Box[F, R, _] @unchecked => JsObject(
      "stepType" -> JsString("box"),
      "uuid" -> JsString(box.id),
      "proof" -> proofFormat.write(box.proof)
    )
    
    case _ => throw NotImplementedError("unreachable case")
  }
}
