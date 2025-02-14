package logicbox.framework

trait Idable {
  def id: String
}

object IdableProof {
  type Step[+F, +R] = Proof.Step[F, R] & Idable

  trait Line[+F, +R] extends Proof.Line[F, R] with Idable {
    // references must also be id'able
    override def refs: List[Step[F, R]]
  }

  trait Box[+F, +R, +I] extends Proof.Box[F, R, I] with Idable {
    // subproof must also be id'able
    override def proof: IdableProof[F, R]
  }
}

type IdableProof[+F, +R] = List[IdableProof.Step[F, R]]
