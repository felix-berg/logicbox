package boxprover

object PLRules {
  import PLFormula.*

  enum Side { case Left; case Right }

  // try to extract a list of `n` formulas from `refs` (only if there are `n`).
  // otherwise report mismatches
  private def extractFormulas(refs: List[ProofStep[PLFormula]]): Either[List[PLFormula], List[Mismatch]] = {
    val ls: List[Either[PLFormula, Mismatch]] = refs.zipWithIndex.map {
      case (ProofLine(formula, rule, refs), _) => 
        Left(formula)
      case (ProofBox(_, _), idx) =>
        Right(ReferenceShouldBeLine(idx))
    }

    val good = ls.forall {
      case Left(_) => true
      case Right(_) => false
    }

    if (good) Left(ls.collect {
      case Left(f) => f
    }) else Right(ls.flatMap {
      case Left(_) => None
      case Right(mm) => Some(mm)
    })
  }

  private def checkCorrectNumberOfRefs(refs: List[ProofStep[PLFormula]], exp: Int): List[Mismatch] =
    if (refs.length != exp) 
      List(WrongNumberOfReferences(exp, refs.length))
    else Nil

  class NullRule extends Rule[PLFormula] {
    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = Nil
  }

  case class Premise() extends NullRule
  case class Assumption() extends NullRule

  case class AndElim(side: Side) extends Rule[PLFormula] {
    private def checkMatchesRef(formula: PLFormula, ref: PLFormula): List[Mismatch] = ref match {
      case And(lhs, rhs) => side match {
        case Side.Left => 
          if (lhs != formula) List(
            FormulaDoesntMatchReference(0, "formula doesn't match left-hand side")
          ) else Nil
            
        case Side.Right =>
          if (rhs != formula) List(
            FormulaDoesntMatchReference(0, "formula doesn't match right-hand side")
          ) else Nil
      }
      
      case _ => List(
        ReferenceDoesntMatchRule(0, "must be conjuction (and)")
      )
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ {
        (extractFormulas(refs): @unchecked) match {
          case Left(List(ref)) =>
            checkMatchesRef(formula, ref)
          case Right(mismatches) => 
            mismatches
        }
      }
    }
  }

  case class AndIntro() extends Rule[PLFormula] {
    private def checkMatchesRef(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Mismatch] = 
      formula match {
        case And(phi, psi) => List(
          (if (phi != r0) List(
            FormulaDoesntMatchReference(0, "left-hand side of formula must match")
          ) else Nil)
          ++
          (if (psi != r1) List(
            FormulaDoesntMatchReference(1, "right-hand side of formula must match")
          ) else Nil)
        ).flatten
        
        case _ => List(FormulaDoesntMatchRule("must be a conjunction (and)"))
      }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 2) ++ {
        (extractFormulas(refs): @unchecked) match {
          case Left(List(r0, r1)) => checkMatchesRef(formula, r0, r1)
          case Right(mismatches) => mismatches
        }
      }
    }
  }

  case class OrIntro(side: Side) extends Rule[PLFormula] {
    private def checkAgainstRef(formula: PLFormula, ref: PLFormula): List[Mismatch] = (side, formula) match {
      case (Side.Left, Or(lhs, _)) => 
        if (lhs != ref) List(
          FormulaDoesntMatchReference(0, "left-hand side of formula must match reference")
        ) else Nil

      case (Side.Right, Or(_, rhs)) =>
        if (rhs != ref) List(
          FormulaDoesntMatchReference(0, "right-hand side of formula must match reference")
        ) else Nil

      case _ => List(FormulaDoesntMatchRule("must be a disjunction (or)"))
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ {
        (extractFormulas(refs): @unchecked) match {
          case Left(List(ref)) => checkAgainstRef(formula, ref)
          case Right(mismatches) => 
            mismatches
        }
      }
    }
  }

  // get first and last elements (same if list has one elemnt)
  private def firstAndLast[T](ls: List[T]): Option[(T, T)] = ls match {
    case Nil => None
    case head :: Nil => Some(head, head)
    case ls => Some(ls.head, ls.last)
  }

  def extractAssumptionConclusion(box: ProofBox[PLFormula, _]): Either[(PLFormula, PLFormula), List[Mismatch]] = {
    firstAndLast(box.proof) match {
      case None => Right(List(MiscellaneousMismatch("box is empty")))

      case Some(ProofLine(asmp, Assumption(), _), ProofLine(concl, _, _)) =>
        Left(asmp, concl)

      case Some(ProofLine(ass, rule, _), _) if rule != Assumption() => Right(List(
        MiscellaneousMismatch("first step in box is not assumption")
      ))

      case Some(ass, concl) => Right({ ass match {
        case _: ProofBox[_, _] => List(
          MiscellaneousMismatch("assumption must be a line")
        )
        case _ => Nil
      }} ++ { concl match {
        case _: ProofBox[_, _] => List(
          MiscellaneousMismatch("conclusion must be a line")
        )
        case _ => Nil
      }})
    }
  }

  case class ImplicationIntro() extends Rule[PLFormula] {
    private def checkMatchesBox(formula: PLFormula, asmp: PLFormula, concl: PLFormula): List[Mismatch] = {
      formula match {
        case Implies(phi, psi) =>
          (if (phi != asmp) List(
            FormulaDoesntMatchReference(0, "left-hand side  must match assumption of box")
          ) else Nil) 
          ++
          (if (psi != concl) List(
            FormulaDoesntMatchReference(0, "right-hand side must match conclusion of box")
          ) else Nil)
        case _ => List(
          FormulaDoesntMatchRule("must be an implication (->)")
        )
      }
    }

    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ {
        refs match {
          case List(box: ProofBox[PLFormula, _] @unchecked) => 
            extractAssumptionConclusion(box) match {
              case Right(mms) => mms
              case Left(asmp, concl) => checkMatchesBox(formula, asmp, concl)
            }
          case _ => Nil
        }
      }
    }
  }
}
