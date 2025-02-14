package logicbox.framework

object Proof {
  sealed trait Step[+F, +R]
  trait Line[+F, +R] extends Step[F, R] {
    def formula: F
    def rule: R
    def refs: List[Step[F, R]]
  }

  trait Box[+F, +R, +I] extends Step[F, R] {
    def info: I
    def proof: Proof[F, R]
  }

  object Line {
    def unapply[F, R](line: Line[F, R]): Option[(F, R, List[Step[F, R]])] =
      Some((line.formula, line.rule, line.refs))
  }

  object Box {
    def unapply[F, R, I](box: Box[F, R, I]): Option[(I, Proof[F, R])] =
      Some((box.info, box.proof))
  }
}

type Proof[+F, +R] = List[Proof.Step[F, R]]
