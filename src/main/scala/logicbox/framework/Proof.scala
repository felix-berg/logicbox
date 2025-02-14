package logicbox.framework

object Proof {
  sealed trait Step[+F, +R]
  case class Line[+F, +R](formula: F, rule: R, refs: List[Step[F, R]]) extends Step[F, R]
  case class Box[+F, +R, +I](info: I, proof: Proof[F, R]) extends Step[F, R]
}

type Proof[+F, +R] = List[Proof.Step[F, R]]
