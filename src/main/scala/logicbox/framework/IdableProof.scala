package logicbox.framework

trait Idable {
  def id: String
}

object IdableProof {
  type Step[+F, +R] = Proof.Step[F, R] & Idable
  type Line[+F, +R] = Proof.Line[F, R] & Idable {
    def refs: List[Step[F, R]]
  }
  type Box[+F, +R, +I] = Proof.Box[F, R, I] & Idable {
    def proof: IdableProof[F, R]
  }
}

type IdableProof[+F, +R] = List[IdableProof.Step[F, R]]
