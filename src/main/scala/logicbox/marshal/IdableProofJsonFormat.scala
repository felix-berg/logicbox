package logicbox.marshal

import spray.json._
import logicbox.framework.IdableProof

case class IdableProofJsonFormat[F, R](stepFormat: JsonFormat[IdableProof.Step[F, R]]) extends RootJsonFormat[IdableProof[F, R]] {
  override def read(json: JsValue): IdableProof[F, R] = ???
  override def write(proof: IdableProof[F, R]): JsValue = JsArray(proof.map(stepFormat.write))
}
