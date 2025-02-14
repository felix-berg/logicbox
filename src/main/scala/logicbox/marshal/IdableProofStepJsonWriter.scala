package logicbox.marshal 

import spray.json._
import logicbox.framework.IdableProof

case class IdableProofStepJsonWriter[F, R](
  ruleToName: R => String,
  formulaToASCII: F => String,
  formulaToLaTeX: F => String,
  getProofWriter: () => JsonWriter[IdableProof[F, R]]
) extends JsonWriter[IdableProof.Step[F, R]] 
{
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
      "proof" -> getProofWriter().write(box.proof)
    )
    
    case _ => throw NotImplementedError("unreachable case")
  }
}
