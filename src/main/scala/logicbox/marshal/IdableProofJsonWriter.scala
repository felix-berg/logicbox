package logicbox.marshal

import spray.json._
import logicbox.framework.IdableProof

case class IdableProofJsonWriter[F, R](stepFormat: JsonWriter[IdableProof.Step[F, R]]) extends RootJsonWriter[IdableProof[F, R]] {
  override def write(proof: IdableProof[F, R]): JsValue = JsArray(proof.map(stepFormat.write))
}
