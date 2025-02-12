package boxprover

sealed trait PropLogicRule extends Rule[PLFormula]
object PropLogicRule {
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

  class NullRule extends PropLogicRule {
    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = Nil
  }

  case class Premise() extends NullRule
  case class Assumption() extends NullRule

  case class AndElim(side: Side) extends PropLogicRule {
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

  case class AndIntro() extends PropLogicRule {
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

  case class OrIntro(side: Side) extends PropLogicRule {
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

  case class OrElim() extends PropLogicRule {
    private def checkAgainstRefs(
      formula: PLFormula, r0: PLFormula, 
      r1: (PLFormula, PLFormula), r2: (PLFormula, PLFormula)
    ): List[Mismatch] = {
      val (as1, cl1) = r1
      val (as2, cl2) = r2

      {
        if (formula != cl1) List(
          FormulaDoesntMatchReference(1, "must match last line of box")
        ) else Nil
      } ++ {
        if (formula != cl2) List(
          FormulaDoesntMatchReference(2, "must match last line of box")
        ) else Nil
      } ++ {
        if (cl1 != cl2) List(
          ReferencesMismatch(List(1, 2), "last lines of boxes must match") 
        ) else Nil
      } ++ { 
        r0 match {
          case Or(lhs, rhs) => {
            if (as1 != lhs) List(
              ReferencesMismatch(List(0, 1), "left-hand side must match assumption")
            ) else Nil
          } ++ {
            if (as2 != rhs) List(
              ReferencesMismatch(List(0, 2), "right-hand side must match assumption")
            ) else Nil
          }
          case _ => List(
            ReferenceDoesntMatchRule(0, "must be a disjunction (or)")
          )
        }
      }
    }

    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      def verifyTypes(r0: ProofStep[PLFormula], r1: ProofStep[PLFormula], r2: ProofStep[PLFormula]): List[Mismatch] = {
        { r0 match {
          case ProofLine(_, _, _) => Nil
          case _ => List(ReferenceShouldBeLine(0))
        }} ++ List((r1, 1), (r2, 2)).flatMap {
          case (ref, idx) => ref match {
            case box: ProofBox[_, _] => Nil
            case _ => List(ReferenceShouldBeBox(idx))
          }
        }
      }

      checkCorrectNumberOfRefs(refs, 3) ++ { refs match {
        case List(r0, r1, r2) => 
          verifyTypes(r0, r1, r2) ++ ((r0, r1, r2) match {
            case (
              ProofLine(r0: PLFormula @unchecked, _, _),
              r1: ProofBox[PLFormula, _] @unchecked,
              r2: ProofBox[PLFormula, _] @unchecked
            ) => 
              List(r1, r2).map(extractAssumptionConclusion) match {
                case List(Left(p1), Left(p2)) => 
                  checkAgainstRefs(formula, r0, p1, p2)

                case List(Right(mms1), Right(mms2)) => mms1 ++ mms2
                case List(_, Right(mms)) => mms
                case List(Right(mms), _) => mms

                case _ => Nil
              }
            case _ => Nil
          })
        case _ => Nil
      }}
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

  case class ImplicationIntro() extends PropLogicRule {
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
          case _ => List(
            ReferenceShouldBeBox(0)
          )
        }
      }
    }
  }

  case class ImplicationElim() extends PropLogicRule {
    private def checkMatchesRef(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Mismatch] = 
      r1 match {
        case Implies(from, to) => 
          (if (from != r0) List(
            ReferencesMismatch(List(0, 1), "must match left-hand side of implication")
          ) else Nil)
          ++
          (if (to != formula) List(
            FormulaDoesntMatchReference(1, "must match right-hand side of implication")
          ) else Nil)
        case _ => List(
          ReferenceDoesntMatchRule(1, "must be an implication")
        )
      }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 2) ++ {
        (extractFormulas(refs): @unchecked) match {
          case Left(List(r0, r1)) =>
            checkMatchesRef(formula, r0, r1)
          case Right(mismatches) => 
            mismatches
        }
      }
    }
  }

  case class NotIntro() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, asmp: PLFormula, concl: PLFormula): List[Mismatch] = {
      {
        if (concl != Contradiction()) List(
          ReferenceDoesntMatchRule(0, "last line of box must be contradiction")
        ) else Nil
      } ++ { formula match {
        case Not(phi) => 
          if (phi != asmp) List(
            FormulaDoesntMatchReference(0, "must be the negation of the assumption in the box")
          ) else Nil
        case _ => List(
          FormulaDoesntMatchRule("must be a negation")
        )
      }
    }}
      
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ { refs match {
        case List(box: ProofBox[PLFormula, _] @unchecked) => 
          extractAssumptionConclusion(box) match {
            case Left(asmp, concl) => checkImpl(formula, asmp, concl)
            case Right(mms) => mms
          }
        case _ => Nil
      }}
    }
  }

  case class NotElim() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Mismatch] = {
      {
        if (formula != Contradiction()) List(
          FormulaDoesntMatchRule("must be contradiction")
        ) else Nil
      } ++ { r1 match {
        case Not(phi) => 
          if (r0 != phi) List(
            ReferencesMismatch(List(0, 1), "second reference must be negation of first")
          ) else Nil
        case _ => List(
          ReferenceDoesntMatchRule(1, "must be negation")
        )
      }}
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = 
      checkCorrectNumberOfRefs(refs, 2) ++ { extractFormulas(refs) match {
        case Left(List(r0, r1)) => checkImpl(formula, r0, r1)
        case Right(mms) => mms
        case _ => Nil
      }}
  }

  case class ContradictionElim() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ { extractFormulas(refs) match {
        case Left(List(r0)) => {
          if (r0 != Contradiction()) List(
            ReferenceDoesntMatchRule(0, "must be a contradiction")
          ) else Nil
        }
        case Right(mms) => mms
        case _ => Nil
      }}
    }
  }

  case class NotNotElim() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ { extractFormulas(refs) match {
        case Left(List(Not(Not(phi)))) => 
          if (formula != phi) List(
            FormulaDoesntMatchReference(0, "must equal the reference, but with the two outer negations removed")
          ) else Nil
        case Left(List(_)) => List(
          ReferenceDoesntMatchRule(0, "must be a negation of a negation")
        )
        case Right(mms) => mms
        case _ => Nil
      }}
    }
  }

  case class ModusTollens() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Mismatch] = {
      {
        formula match {
          case Not(_) => Nil
          case _ => List(FormulaDoesntMatchRule("must be a negation"))
        }
      } ++ {
        r0 match {
          case Implies(_, _) => Nil
          case _ => List(ReferenceDoesntMatchRule(0, "must be an implication"))
        }
      } ++ {
        r1 match {
          case Not(_) => Nil
          case _ => List(ReferenceDoesntMatchRule(1, "must be a negation"))
        }
      } ++ {
        (formula, r0, r1) match {
          case (Not(phi2), Implies(phi1, psi1), Not(psi2)) => {
            if (phi2 != phi1) List(
              FormulaDoesntMatchReference(0, "must be negation of left-hand side of implication")
            ) else Nil
          } ++ {
            if (psi1 != psi2) List(
              ReferencesMismatch(List(0, 1), "second reference must be the negation of the right-hand side of the implication")
            ) else Nil
          }
          case _ => Nil
        }
      }
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 2) ++ { extractFormulas(refs) match {
        case Left(List(r0, r1)) => checkImpl(formula, r0, r1)
        case Right(mms) => mms
        case _ => Nil
      }}
    }
  }

  case class NotNotIntro() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, ref: PLFormula): List[Mismatch] = {
      formula match {
        case Not(Not(phi)) => 
          if (phi != ref) List(
            FormulaDoesntMatchReference(0, "must be ")
          ) else Nil
        case _ => List(
          FormulaDoesntMatchRule("must equal the reference, but with the two outer negations removed")
        )
      }
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ { extractFormulas(refs) match {
        case Left(List(ref)) => checkImpl(formula, ref)
        case Right(mms) => mms
        case _ => Nil
      }}
    }
  }
}
