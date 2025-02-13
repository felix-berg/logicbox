package boxprover

sealed trait PropLogicRule extends Rule[PLFormula] {
  type V = PropLogicViolation
}

object PropLogicRule {
  import PLFormula.*
  import PropLogicViolation.*

  enum Side { case Left; case Right }

  type Violation = PropLogicViolation

  // try to extract a list of `n` formulas from `refs` (only if there are `n`).
  // otherwise report mismatches

  private enum BoxOrLine { case Box; case Line }
  private def extractAndThen(refs: List[ProofStep[PLFormula]], pattern: Seq[BoxOrLine]) 
    (func: PartialFunction[List[ProofStep[PLFormula]], List[Violation]]): List[Violation] = 
  {
    def checkLengthMatches(refs: Seq[_], pattern: Seq[_]): List[Violation] = {
      if (refs.length != pattern.length) List(
        WrongNumberOfReferences(pattern.length, refs.length)
      ) else Nil
    }

    def extract(refs: List[ProofStep[PLFormula]], pattern: Seq[BoxOrLine]): Either[List[ProofStep[PLFormula]], List[Violation]] = {
      val zp = refs.zipWithIndex.zip(pattern).map { case ((ref, idx), pattern) => (idx, pattern, ref)}

      val result = zp.map {
        // matches
        case (_, BoxOrLine.Line, line: ProofLine[_]) => Left(line) 
        case (_, BoxOrLine.Box, box: ProofBox[_, _]) => Left(box)

        // violations
        case (idx, BoxOrLine.Box, line: ProofLine[_]) => Right(ReferenceShouldBeBox(idx))
        case (idx, BoxOrLine.Line, box: ProofBox[_, _]) => Right(ReferenceShouldBeLine(idx))
      }

      val good = result.forall {
        case Left(_) => true
        case Right(_) => false
      }

      if (good) {
        // collect steps 
        Left(result.collect { case Left(step) => step }) 
      } else {
        // collect violations
        Right(result.collect { case Right(mm) => mm })
      }
    }

    checkLengthMatches(refs, pattern) ++ { 
      extract(refs, pattern) match {
        case Left(ls: List[ProofStep[PLFormula]]) =>
          assert(func.isDefinedAt(ls), s"Partial function is defined on given pattern $pattern")
          func.apply(ls)
        case Right(mismatches) => mismatches
      }
    }
  }

  private def extractNFormulasAndThen(refs: List[ProofStep[PLFormula]], n: Int)
    (func: PartialFunction[List[PLFormula], List[Violation]]): List[Violation] = 
  {
    extractAndThen(refs, (1 to n).map { s => BoxOrLine.Line }) {
      case lines: List[ProofLine[PLFormula]] @unchecked => 
        func.apply(lines.map(_.formula))
    }
  }
  

  class NullRule extends PropLogicRule {
    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = Nil
  }

  case class Premise() extends NullRule
  case class Assumption() extends NullRule

  case class AndElim(side: Side) extends PropLogicRule {
    private def checkImpl(formula: PLFormula, ref: PLFormula): List[Violation] = ref match {
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(ref) => checkImpl(formula, ref)
      }
    }
  }

  case class AndIntro() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Violation] = 
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 2) {
        case List(r0, r1) => checkImpl(formula, r0, r1)
      }
    }
  }

  case class OrIntro(side: Side) extends PropLogicRule {
    private def checkImpl(formula: PLFormula, ref: PLFormula): List[Violation] = (side, formula) match {
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(ref) => checkImpl(formula, ref)
      }
    }
  }

  case class OrElim() extends PropLogicRule {
    private def checkImpl(
      formula: PLFormula, r0: PLFormula, 
      r1: (PLFormula, PLFormula), r2: (PLFormula, PLFormula)
    ): List[Violation] = {
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

    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      import BoxOrLine._
      val pattern = List(Line, Box, Box)

      extractAndThen(refs, pattern)  {
        case List(
          ProofLine(r0: PLFormula, _, _), 
          r1: ProofBox[PLFormula, _] @unchecked, 
          r2: ProofBox[PLFormula, _] @unchecked) => 
          List(r1, r2).map(extractAssumptionConclusion) match {
            case List(Left(p1), Left(p2)) => 
              checkImpl(formula, r0, p1, p2)

            case List(Right(vs1), Right(vs2)) => vs1 ++ vs2
            case List(_, Right(vs)) => vs
            case List(Right(vs), _) => vs

            case _ => Nil
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

  def extractAssumptionConclusion(box: ProofBox[PLFormula, _]): Either[(PLFormula, PLFormula), List[Violation]] = {
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
    private def checkMatchesBox(formula: PLFormula, asmp: PLFormula, concl: PLFormula): List[Violation] = {
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

    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractAndThen(refs, List(BoxOrLine.Box)) {
        case List(box: ProofBox[PLFormula, _] @unchecked) => 
          extractAssumptionConclusion(box) match {
            case Right(mms) => mms
            case Left(asmp, concl) => checkMatchesBox(formula, asmp, concl)
          }
      }
    }
  }

  case class ImplicationElim() extends PropLogicRule {
    private def checkMatchesRef(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Violation] = 
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 2) {
        case List(r0, r1) =>
          checkMatchesRef(formula, r0, r1)
      }
    }
  }

  case class NotIntro() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, asmp: PLFormula, concl: PLFormula): List[Violation] = {
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
      
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractAndThen(refs, List(BoxOrLine.Box)) {
        case List(box: ProofBox[PLFormula, _] @unchecked) => 
          extractAssumptionConclusion(box) match {
            case Left(asmp, concl) => checkImpl(formula, asmp, concl)
            case Right(mms) => mms
          }
      }
    }
  }

  case class NotElim() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Violation] = {
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = 
      extractNFormulasAndThen(refs, 2) {
        case List(r0, r1) => checkImpl(formula, r0, r1)
      }
  }

  case class ContradictionElim() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(r0) =>
          if (r0 != Contradiction()) List(
            ReferenceDoesntMatchRule(0, "must be a contradiction")
          ) else Nil
      }
    }
  }

  case class NotNotElim() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(Not(Not(phi))) => 
          if (formula != phi) List(
            FormulaDoesntMatchReference(0, "must equal reference with the two outermost negations removed")
          ) else Nil
        case List(_) => List(
          ReferenceDoesntMatchRule(0, "must be a negation of a negation")
        )
      }
    }
  }

  case class ModusTollens() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, r0: PLFormula, r1: PLFormula): List[Violation] = {
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 2) {
        case List(r0, r1) => checkImpl(formula, r0, r1)
      }
    }
  }

  case class NotNotIntro() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, ref: PLFormula): List[Violation] = {
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

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(ref) => checkImpl(formula, ref)
      }
    }
  }

  case class ProofByContradiction() extends PropLogicRule {
    private def checkImpl(formula: PLFormula, asmp: PLFormula, concl: PLFormula): List[Violation] = {
      { asmp match {
        case Not(phi) => 
          if (formula != phi) List(
            FormulaDoesntMatchReference(0, "must be assumption without negation")
          ) else Nil
        case _ => List(ReferenceDoesntMatchRule(0, "assumption in box must be a negation"))
      }} ++ { concl match {
        case Contradiction() => Nil
        case _ => List(ReferenceDoesntMatchRule(0, "last line in box must be a contradiction"))
      }}
    }

    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractAndThen(refs, List(BoxOrLine.Box)) {
        case List(box: ProofBox[PLFormula, _] @unchecked) => 
          extractAssumptionConclusion(box) match {
            case Left(asmp, concl) => checkImpl(formula, asmp, concl)
            case Right(mms) => mms
          }
      }
    }
  }

  case class LawOfExcludedMiddle() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 0) {
        case Nil =>
          formula match {
            case Or(lhs, Not(rhs)) if lhs == rhs => Nil
            case _ => List(FormulaDoesntMatchRule("must be the disjunction of a formula and its negation"))
          }
      }
    }
  }

  case class Copy() extends PropLogicRule {
    override def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Violation] = {
      extractNFormulasAndThen(refs, 1) {
        case List(ref) => 
          if (ref != formula) List(
            FormulaDoesntMatchReference(0, "must be an exact copy of reference")
          ) else Nil
      }
    }
  }
}
