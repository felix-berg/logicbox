package logicbox.proof

import logicbox.framework.{Proof, ModifiableProof}
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
  private type D = ModifiableProof.Error[Id]
  private type Pf = ModifiableProof[Option[F], Option[R], Option[B], Id]
  private type NotFound = Proof.StepNotFound[Id]
  private type PStep = Proof.Step[Option[F], Option[R], Option[B], Id]

  private def getStepImpl(id: Id): Option[ProofImpl.Step[F, R, B, Id]] = steps.get(id)

  override def getStep(id: Id): Either[NotFound, PStep] = {
    getStepImpl(id)
      .collect{ case s: PStep @unchecked => s } // will always satisfy Proof.Step
      .toRight(Proof.StepNotFound(id, "no such step"))
  }

  private case class InsertResult(
    newStepSeq: List[Id], 
    modifiedSteps: List[(Id, ProofImpl.Step[F, R, B, Id])] = Nil
  )

  private def insertIdAtIdx(idToInsert: Id, idx: Int, dir: Direction, inList: List[Id]): InsertResult = {
    assert(inList.length > idx)
    val (before, elm :: after) = inList.splitAt(idx): @unchecked // unchecked because elm has index `idx`, so always suceeds
    dir match {
      case Direction.Above => InsertResult(
        newStepSeq = before ++ (idToInsert :: elm :: after)
      )
      case Direction.Below => InsertResult(
        newStepSeq = (before :+ elm) ++ (idToInsert :: after)
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
            InsertResult(stepSeq, stepsToBeAdded) <- insertIdAtLine(idToInsert, whereId, dir, steps)
            box = Box(info, stepSeq)
          } yield InsertResult(inList, (stepId -> box) :: stepsToBeAdded)
          case _ => None
        }
      }
    }
  
  private def insertId(id: Id, where: Pos[Id]): Either[Error[Id], InsertResult] = where match {
    case ProofTop => Right(InsertResult(newStepSeq = id :: rootSteps))

    case AtLine(whereId: Id, dir) => 
      insertIdAtLine(id, whereId, dir, rootSteps)
        .toRight(InvalidPosition(where, "no line with given id"))

    case BoxTop(boxId) => for {
      step <- getStepImpl(boxId).toRight(InvalidPosition(where, "no box with given id"))
      Box(info, boxSteps) <- step match {
        case b: Box[B, Id] => Right(b)
        case _ => Left(InvalidPosition(where, "id is a line, not a box"))
      }
      newBox = ProofImpl.Box(info, id :: boxSteps)
      stepsToBeAdded = List(boxId -> newBox)
    } yield InsertResult(newStepSeq = rootSteps, stepsToBeAdded)
  }

  private def insertStep(
    id: Id, step: ProofImpl.Step[F, R, B, Id], where: Pos[Id]
  ): Either[Error[Id], Pf] = for {
    InsertResult(newRootSteps, modifiedSteps) <- insertId(id, where)
    newSteps = steps + (id -> step) ++ modifiedSteps
  } yield ProofImpl(rootSteps = newRootSteps, steps = newSteps)

  override def addLine(id: Id, where: Pos[Id]): Either[Error[Id], Pf] =
    insertStep(id, ProofImpl.Line(None, None, Nil), where)

  override def addBox(id: Id, where: Pos[Id]): Either[Error[Id], Pf] =
    insertStep(id, ProofImpl.Box(None, Nil), where)

  private def getCurrentLine(lineId: Id): Either[Error[Id], ProofImpl.Line[F, R, Id]] = for {
    step <- getStepImpl(lineId).toRight(CannotUpdateStep(lineId, "no step with given id"))
    line <- step match {
      case line: Line[F, R, Id] => Right(line)
      case _ => Left(CannotUpdateStep(lineId, "step is not a line"))
    }
  } yield line

  private def updateStep(stepId: Id, newStep: ProofImpl.Step[F, R, B, Id]): Pf =
    ProofImpl(rootSteps = rootSteps, steps = steps + (stepId -> newStep))

  override def updateFormula(lineId: Id, formula: Option[F]): Either[Error[Id], Pf] = for {
    line <- getCurrentLine(lineId)
    newStep = Line(formula, line.rule, line.refs)
  } yield updateStep(lineId, newStep)

  override def updateRule(lineId: Id, rule: Option[R]): Either[Error[Id], Pf] = for {
    line <- getCurrentLine(lineId)
    newStep = Line(line.formula, rule, line.refs)
  } yield updateStep(lineId, newStep)

  override def updateReferences(lineId: Id, refs: List[Id]): Either[Error[Id], Pf] = for {
    line <- getCurrentLine(lineId)
    newStep = Line(line.formula, line.rule, refs)
  } yield updateStep(lineId, newStep)

  private case class RemoveResult(
    newStepSeq: List[Id], 
    modifiedSteps: List[(Id, ProofImpl.Step[F, R, B, Id])]
  )

  private def removeIdFromProofStructure(idToRemove: Id, inList: List[Id]): Option[RemoveResult] =
    inList.indexOf(idToRemove) match {
      case idx if idx != -1 => 
        val (bef, elm :: aft) = inList splitAt idx: @unchecked
        Some(RemoveResult(bef ++ aft, Nil))

      case _ => inList.foldLeft(None) {
        case (Some(res), _) => Some(res)
        case (None, stepId) => for {
          step <- getStepImpl(stepId)
          Box(info, boxSteps) <- step match {
            case b: Box[B, Id] => Some(b)
            case _ => None
          }
          RemoveResult(newStepSeq, modifiedSteps) <- removeIdFromProofStructure(idToRemove, boxSteps)
          newBox = Box(info, newStepSeq)
        } yield RemoveResult(newStepSeq, modifiedSteps :+ (stepId -> newBox))
      }
    }

  override def removeStep(id: Id): Either[Error[Id], Pf] = for {
    RemoveResult(newStepSeq, modifiedSteps) <- 
      removeIdFromProofStructure(id, rootSteps).toRight(CannotRemoveStep(id, "step not found"))
  } yield ProofImpl(rootSteps = newStepSeq, steps = steps.removed(id) ++ modifiedSteps)
}
