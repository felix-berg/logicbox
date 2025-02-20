package logicbox.proof

import logicbox.framework.{Proof, ModifiableProof}
import logicbox.framework.Proof.Step
import logicbox.framework.Proof.StepNotFound

object ProofImpl {
  sealed trait Step[+F, +R, +B, +Id]

  case class Line[+F, +R, +Id](formula: F, rule: R, refs: List[Id]) 
    extends Step[F, R, Nothing, Id] with Proof.Line[F, R, Id]
  case class Box[+B, +Id](info: B, steps: List[Id]) 
    extends Step[Nothing, Nothing, B, Id] with Proof.Box[B, Id]

  def empty[F, R, B, Id]: ProofImpl[F, R, B, Id] = 
    ProofImpl(steps = Map.empty, rootSteps = Nil)
}

case class ProofImpl[F, R, B, Id](
  steps: Map[Id, Step[F, R, B, Id]],
  rootSteps: List[Id],
) extends Proof[F, R, B, Id] with ModifiableProof[F, R, B, Id]
{
  import ModifiableProof._
  private type D = ModifiableProof.Diagnostic[Id]
  private type Pf = ModifiableProof[F, R, B, Id]

  override def getStep(id: Id): Either[StepNotFound[Id], Step[F, R, B, Id]] = ???
  override def addLine(id: Id, where: Pos[Id]): Either[List[D], Pf] = ???
  override def addBox(id: Id, where: Pos[Id]): Either[List[D], Pf] = ???
  override def removeStep(id: Id): Either[List[D], Pf] = ???
  override def updateFormula(lineId: Id, formula: F): Either[List[D], Pf] = ???
  override def updateRule(lineId: Id, rule: R): Either[List[D], Pf] = ???
  override def updateReference(lineId: Id, refIdx: Int, refId: Id): Either[List[D], Pf] = ???
}
