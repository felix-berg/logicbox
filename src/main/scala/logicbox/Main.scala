package logicbox

import logicbox.framework._
import logicbox.Proof.StepNotFound

object Proof {
  sealed trait Step[+Formula, +Rule, +BoxInfo, +Id]
  trait Line[+Formula, +Rule, +Id] extends Step[Formula, Rule, Nothing, Id] {
    def formula: Formula
    def rule: Rule
    def refs: Seq[Id]
  }

  trait Box[+BoxInfo, +Id] extends Step[Nothing, Nothing, BoxInfo, Id] {
    def info: BoxInfo
    def lines: Seq[Id]
  }

  object Line {
    def unapply[F, R, I](line: Line[F, R, I]): Option[(F, R, Seq[I])] =
      Some((line.formula, line.rule, line.refs))
  }

  object Box {
    def unapply[B, I](box: Box[B, I]): Option[(B, Seq[I])] =
      Some((box.info, box.lines))
  }

  case class StepNotFound[Id](id: Id, expl: String)
}

trait Proof[+Formula, +Rule, +BoxInfo, Id] {
  import Proof._

  def getStep(id: Id): Either[StepNotFound[Id], Step[Formula, Rule, BoxInfo, Id]]
  def roots: Seq[Id]
}

case class LineImpl[+F](formula: F) extends Reference.Line[F]
case class BoxImpl[+F, +B](info: B, assumption: F, conclusion: F) extends Reference.Box[F, B]

object Verifier {
  sealed trait Result[+V, +Id] {
    def expl: String
  }

  case class RuleViolation[+Id, +V](where: Id, violation: V) extends Result[Id, V] {
    def expl = "TODO, missing"
  }

  case class MissingReference[+Id](where: Id, whichRef: Int, refId: Id, expl: String) extends Result[Id, Nothing]
  case class MissingStep[+Id](id: Id, expl: String) extends Result[Id, Nothing]
  case class MalformedFormula[+Id](where: Id, expl: String) extends Result[Id, Nothing] 
  case class MissingRule[+Id](where: Id, expl: String) extends Result[Id, Nothing]
  case class MalformedReference[+Id](where: Id, whichRef: Int, refId: Id, expl: String) extends Result[Id, Nothing]
}

case class Verifier[F, R, B, Id]() {
  type Pf = Proof[F, R, B, Id]
  type Ch[V] = RuleChecker[F, R, B, V]

  import Verifier._
  type Res[V] = Result[Id, V]

  private def resolveBox(stepId: Id, whichRef: Int, boxId: Id, proof: Pf, box: Proof.Box[B, Id]):
    Either[List[Res[Nothing]], Reference.Box[F, B]] = for {
      _ <- if (box.lines.isEmpty) {
        Left(List(MalformedReference(stepId, whichRef, boxId, "box is empty")))
      } else Right(())

      ids = List(box.lines.head, box.lines.last)
      names = List("assumption", "conclusion") // for err msg
      optRefs = ids.map(proof.getStep)

      _ <- (names, ids, optRefs).zipped.collect {
        case (which, id, Left(StepNotFound(_, expl))) => 
          MalformedReference(stepId, whichRef, boxId, s"$which (id $id) in box not found: $expl")

        case (which, id, Right(Proof.Box(_, _))) => 
          MalformedReference(stepId, whichRef, boxId, s"$which (id: $id) in box is a box (should be a line)")
      }.toList match {
        case Nil => Right(())
        case l => Left(l)
      }

      List(ass, concl) = optRefs
        .collect { case Right(t) => t }
        .map(_.asInstanceOf[Proof.Line[F, R, B]])
        .map(_.formula)

    } yield BoxImpl(box.info, ass, concl)

  private def resolveRefs(stepId: Id, proof: Pf, refs: Seq[Id]): Either[List[Res[Nothing]], List[Reference[F, B]]] =
    val optRefs = refs.map(proof.getStep)
    val idxs = (0 until optRefs.length)
    val results = (refs, optRefs, idxs).zipped.toList.collect {
      case (refId, Left(StepNotFound(_, expl)), idx) =>
        Left(List(MissingReference(stepId, idx, refId, expl)))

      case (_, Right(Proof.Line(formula: F @unchecked, _, _)), _)   => Right(LineImpl(formula))
      case (refId, Right(b: Proof.Box[B, Id]), idx)      => resolveBox(stepId, idx, refId, proof, b)
    }
    val hasBadness = results.exists {
      case Left(_) => true
      case _ => false
    }
    if (hasBadness) {
      Left(
        results.collect { case Left(t) => t }.flatten
      )
    } else {
      Right(
        results.collect { case Right(t) => (t: Reference[F, B]) }
      )
    }

  private def verifySteps[V](proof: Pf, ids: Seq[Id], ruleChecker: Ch[V]): List[Res[V]] =
    ids.map(id => (id, proof.getStep(id))).flatMap {
      case (id, Right(Proof.Line(formula: F @unchecked, rule: R @unchecked, refs: Seq[Id] @unchecked))) => 
        resolveRefs(id, proof, refs) match {
          case Right(refs) => 
            ruleChecker.check(rule, formula, refs)
              .map(viol => RuleViolation(id, viol))
          case Left(results) => results
        }

      case (id, Right(Proof.Box(_, lines: Seq[Id] @unchecked))) => 
        verifySteps[V](proof, lines, ruleChecker)

    }.toList

  def verify[V](proof: Proof[F, R, B, Id], ruleChecker: RuleChecker[F, R, B, V]): List[Result[V]] =
    verifySteps(proof, proof.roots, ruleChecker)
}

object Main {
  def main(args: Array[String]): Unit = {
  }
}
