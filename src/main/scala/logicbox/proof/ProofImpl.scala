package logicbox.proof

import logicbox.framework.{Proof, ModifiableProof}
import logicbox.framework.ModifiableProof.Diagnostic
import logicbox.framework.ModifiableProof.Pos

object ProofImpl {
  sealed trait Step[+F, +R, +B, +Id]

  case class Line[+F, +R, +Id](formula: Option[F], rule: Option[R], refs: List[Id]) 
    extends Step[F, R, Nothing, Id] with Proof.Line[Option[F], Option[R], Id]
    
  case class Box[+B, +Id](info: Option[B], steps: List[Id]) 
    extends Step[Nothing, Nothing, B, Id] with Proof.Box[Option[B], Id]

  def empty[F, R, B, Id]: ProofImpl[F, R, B, Id] = 
    ProofImpl(steps = Map.empty, rootSteps = Nil)
}

case class ProofImpl[F, R, B, Id](
  steps: Map[Id, ProofImpl.Step[F, R, B, Id]],
  rootSteps: List[Id],
) extends ModifiableProof[Option[F], Option[R], Option[B], Id]
{
  import ModifiableProof._
  private type D = ModifiableProof.Diagnostic[Id]
  private type Pf = ModifiableProof[Option[F], Option[R], Option[B], Id]
  private type NotFound = Proof.StepNotFound[Id]
  private type PStep = Proof.Step[Option[F], Option[R], Option[B], Id]

  override def getStep(id: Id): Either[NotFound, PStep] = {
    steps.get(id) match {
      case Some(step: PStep) => Right(step)
      case None => Left(Proof.StepNotFound(id, "no such step"))
    }
  }

  private def insertId(
    idToInsert: Id, whereId: Id, dir: Direction, 
    inList: List[Id], allSteps: Map[Id, ProofImpl.Step[F, R, B, Id]]
  ): Pf = 
    inList.indexOf(whereId) match {
      case -1 => ???
      case idx => 
        val (before, elm :: after) = inList.splitAt(idx): @unchecked // unchecked because elm has index `idx`, so always suceeds
        dir match {
          case Direction.Above => ProofImpl(
            steps = allSteps,
            rootSteps = before ++ (idToInsert :: elm :: after)
          )
          case Direction.Below => ProofImpl(
            steps = allSteps,
            rootSteps = (before :+ elm) ++ (idToInsert :: after)
          )
        }
    }
  
  private def insertId(id: Id, where: Pos[Id], allSteps: Map[Id, ProofImpl.Step[F, R, B, Id]]): Pf = where match {
    case Top => ProofImpl(
      steps = allSteps,
      rootSteps = id :: rootSteps
    )

    case IdPos(whereId: Id, dir) =>
      insertId(id, whereId, dir, rootSteps, allSteps)
  }
    
  override def addLine(id: Id, where: Pos[Id]): Either[List[D], Pf] =  {
    val step = ProofImpl.Line(None, None, Nil)
    val newSteps = steps + (id -> step)
    Right(insertId(id, where, newSteps))
  }

  override def addBox(id: Id, where: Pos[Id]): Either[List[D], Pf] = ???
  override def updateReference(lineId: Id, refIdx: Int, refId: Id): Either[List[D], Pf] = ???
  override def updateFormula(lineId: Id, formula: Option[F]): Either[List[D], Pf] = ???
  override def removeStep(id: Id): Either[List[D], Pf] = ???
  override def updateRule(lineId: Id, rule: Option[R]): Either[List[D], Pf] = ???
}
