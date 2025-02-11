package boxprover

import boxprover.PLFormula.Contradiction
import boxprover.PLFormula.Tautology
import boxprover.PLFormula.Atom
import boxprover.PLFormula.And
import boxprover.PLFormula.Or
import boxprover.PLFormula.Implies
import boxprover.PLFormula.Not

object PLValidator {
  private def freeVars(phi: PLFormula): Set[Char] = phi match {
    case Contradiction() | Tautology() => Set.empty
    case Atom(c) => Set(c)
    case And(phi, psi) => freeVars(phi) ++ freeVars(psi)
    case Or(phi, psi) => freeVars(phi) ++ freeVars(psi)
    case Implies(phi, psi) => freeVars(phi) ++ freeVars(psi)
    case Not(phi) => freeVars(phi)
  }
  
  private def generateValuations(vars: Set[Char]): Iterator[Map[Char, Boolean]] =
    val sadMap = vars.map(c => (c, false)).toMap
    vars.subsets.map(set =>
      set.foldLeft(sadMap) { case (m, c) => m + (c -> true) }
    )

  private def validateVisit(phi: PLFormula, valuation: Map[Char, Boolean]): Boolean = phi match {
    case Contradiction() => false
    case Tautology() => true
    case Atom(c) => valuation.getOrElse(c, ???)
    case And(phi, psi) => validateVisit(phi, valuation) && validateVisit(psi, valuation)
    case Or(phi, psi) => validateVisit(phi, valuation) || validateVisit(psi, valuation)
    case Implies(phi, psi) => !validateVisit(phi, valuation) || validateVisit(psi, valuation)
    case Not(phi) => !validateVisit(phi, valuation)
  }
  
  def validate(phi: PLFormula): Boolean =
    generateValuations(freeVars(phi)).forall(vlt => validateVisit(phi, vlt))
}
