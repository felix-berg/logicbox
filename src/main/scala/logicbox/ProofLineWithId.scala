package logicbox

import logicbox.framework.Proof

import java.util.Objects

class ProofLineWithId[F, R](
  val formula: F,
  val rule: R,
  val refs: List[Proof.Step[F, R]],
  val id: String
) extends Proof.Line[F, R] {
  override def equals(other: Any): Boolean = other match {
    case oth: ProofLineWithId[_, _] =>
      formula == oth.formula &&
      rule == oth.rule &&
      refs == oth.refs &&
      id == oth.id
    case _ => false
  }
  override def hashCode: Int = Objects.hash(formula, rule, refs, id)
  override def toString: String = s"[id: $id, ${super.toString}]"
}
