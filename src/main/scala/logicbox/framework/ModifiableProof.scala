package logicbox.framework

object ModifiableProof {
  sealed trait Diagnostic[+Id]

  enum Direction { case Above; case Below }
  case object Root
  case class Pos[Id](id: Id | Root.type, dir: Direction)
}

trait ModifiableProof[F, R, B, Id] {
  import ModifiableProof._

  private type Pf = ModifiableProof[F, R, B, Id]
  private type D = Diagnostic[Id]

  def addLine(id: Id, where: Pos[Id]): Either[List[D], Pf]
  def addBox(id: Id, where: Pos[Id]): Either[List[D], Pf]

  def updateFormula(lineId: Id, formula: F): Either[List[D], Pf]
  def updateRule(lineId: Id, rule: R): Either[List[D], Pf]
  def updateReference(lineId: Id, refIdx: Int, refId: Id): Either[List[D], Pf]

  def removeStep(id: Id): Either[List[D], Pf]
}
