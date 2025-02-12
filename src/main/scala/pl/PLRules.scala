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
}
