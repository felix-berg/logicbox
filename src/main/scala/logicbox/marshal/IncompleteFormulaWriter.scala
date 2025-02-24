package logicbox.marshal

import spray.json._

case class IncompleteFormula[F](
  userInput: String, optFormula: Option[F]
)

class IncompleteFormulaWriter[F](
  toLaTeX: F => String, toASCII: F => String
) extends JsonWriter[IncompleteFormula[F]] {

  override def write(obj: IncompleteFormula[F]): JsValue = JsObject(
    "userInput" -> JsString(obj.userInput),
    "ascii" -> obj.optFormula.map(toASCII).map(JsString.apply).getOrElse(JsNull),
    "latex" -> obj.optFormula.map(toLaTeX).map(JsString.apply).getOrElse(JsNull),
  )

}
