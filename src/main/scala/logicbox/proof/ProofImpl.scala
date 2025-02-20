package logicbox.proof

import logicbox.framework.{Proof, ModifiableProof}
import logicbox.framework.ModifiableProof.Diagnostic
import logicbox.framework.ModifiableProof.Pos
import logicbox.proof.ProofImpl.Line
import logicbox.proof.ProofImpl.Box

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
      case Some(step: PStep @unchecked) => Right(step)
      case None => Left(Proof.StepNotFound(id, "no such step"))
    }
  }

  private case class InsertResult(stepSeq: List[Id], modifiedSteps: List[(Id, ProofImpl.Step[F, R, B, Id])] = Nil)

  private def insertIdAtIdx(idToInsert: Id, idx: Int, dir: Direction, inList: List[Id]): InsertResult = {
    assert(inList.length > idx)
    val (before, elm :: after) = inList.splitAt(idx): @unchecked // unchecked because elm has index `idx`, so always suceeds
    dir match {
      case Direction.Above => InsertResult(
        stepSeq = before ++ (idToInsert :: elm :: after)
      )
      case Direction.Below => InsertResult(
        stepSeq = (before :+ elm) ++ (idToInsert :: after)
      )
    }
  }

  private def insertIdAtLine(idToInsert: Id, whereId: Id, dir: Direction, inList: List[Id]): Option[InsertResult] = 
    inList.indexOf(whereId) match {
      case idx if idx != -1 => Some(insertIdAtIdx(idToInsert, idx, dir, inList))

      case _ => inList.foldLeft(None) {
        case (Some(res), _) => Some(res) // when found, 'break'
        case (None, stepId) => steps(stepId) match {
          case Box(info, steps) => for {
            InsertResult(stepSeq, modifiedSteps) <- insertIdAtLine(idToInsert, whereId, dir, steps)
            box = Box(info, stepSeq)
          } yield InsertResult(inList, (stepId -> box) :: modifiedSteps)
          case _ => None
        }
      }
    }
  
  private def insertId(id: Id, where: Pos[Id]): Either[List[D], InsertResult] = where match {
    case ProofTop => Right(InsertResult(stepSeq = id :: rootSteps))

    case AtLine(whereId: Id, dir) => 
      insertIdAtLine(id, whereId, dir, rootSteps)
        .toRight(List(PositionNotFound(where)))

    case BoxTop(boxId) => for {
      step <- steps.get(boxId).toRight(???) // (PositionNotFound(where))
      Box(info, boxSteps) <- step match {
        case b: Box[B, Id] => Right(b)
        case _ => Left(???)
      }
      newBox = ProofImpl.Box(info, id :: boxSteps)
      modifiedSteps = List(boxId -> newBox)
    } yield InsertResult(stepSeq = rootSteps, modifiedSteps)
  }

  private def insertStep(id: Id, step: ProofImpl.Step[F, R, B, Id], where: Pos[Id]): Either[List[D], Pf] =
    for {
      InsertResult(newRootSteps, modifiedSteps) <- insertId(id, where)
      newSteps = steps + (id -> step) ++ modifiedSteps
    } yield ProofImpl(rootSteps = newRootSteps, steps = newSteps)
    
  override def addLine(id: Id, where: Pos[Id]): Either[List[D], Pf] =
    insertStep(id, ProofImpl.Line(None, None, Nil), where)

  override def addBox(id: Id, where: Pos[Id]): Either[List[D], Pf] =
    insertStep(id, ProofImpl.Box(None, Nil), where)

  override def updateReference(lineId: Id, refIdx: Int, refId: Id): Either[List[D], Pf] = ???
  override def updateFormula(lineId: Id, formula: Option[F]): Either[List[D], Pf] = ???
  override def removeStep(id: Id): Either[List[D], Pf] = ???
  override def updateRule(lineId: Id, rule: Option[R]): Either[List[D], Pf] = ???
}
