package logicbox.framework

sealed trait Reference[+F, +I]

object Reference {
  trait Formula[+F] extends Reference[F, Nothing] {
    def formula: F
  }
  trait Box[+F, +I] extends Reference[F, I] {
    def info: I
    def lines: Seq[F]
  }

  object Formula {
    def unapply[F](f: Formula[F]): Option[(F)] =
      Some(f.formula)
  }

  object Box {
    def unapply[F, I](b: Box[F, I]): Option[(I, Seq[F])] =
      Some(b.info, b.lines)
  }
}

trait RuleChecker[F, R, I, V] {
  def check(rule: R, formula: F, refs: List[Reference[F, I]]): List[V]
}
