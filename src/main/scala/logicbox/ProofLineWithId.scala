package logicbox

import logicbox.framework.Proof

import java.util.Objects

class ProofLineWithId[F, R](
  override val formula: F,
  override val rule: R,
  override val refs: List[Proof.Step[F, R]],
  val id: String
) extends Proof.Line[F, R](formula, rule, refs) {
  override def equals(other: Any): Boolean = other match {
    case oth: ProofLineWithId[_, _] => super.equals(oth) && oth.id == id
    case _ => false
  }
  override def hashCode: Int = Objects.hash(super.hashCode, id)
  override def toString: String = s"[id: $id, ${super.toString}]"
}
