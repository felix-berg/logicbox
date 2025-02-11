package boxprover

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    val lexer = PLLexer()
    val parser = PLParser()
    val tokens = lexer.apply("(((p -> q) -> r) and (s -> not p) and (t) and (not s and t -> q)) -> r")
    println(tokens)
    val formula = parser.apply(tokens)
    println(formula)
    println(s"Formula is ${if (PLValidator.validate(formula)) then "valid" else "invalid"}")
  }
}
