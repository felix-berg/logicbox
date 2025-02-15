package logicbox

import logicbox.formula.PLFormula
import logicbox.formula.PLFormula.Contradiction
import logicbox.formula.PLFormula.Tautology
import logicbox.formula.PLFormula.Atom
import logicbox.formula.PLFormula.And
import logicbox.formula.PLFormula.Or
import logicbox.formula.PLFormula.Implies
import logicbox.formula.PLFormula.Not

object StupidPLFormulaToLatex {
  def formulaToLaTeX(formula: PLFormula): String = formula match {
    case Contradiction() => "\\bot"
    case Tautology() => "\\top"
    case Atom(c) => c.toString
    case And(phi, psi) => formulaToLaTeX(phi) + " \\land " + formulaToLaTeX(psi)
    case Or(phi, psi) => formulaToLaTeX(phi) + " \\lor " + formulaToLaTeX(psi)
    case Implies(phi, psi) => formulaToLaTeX(phi) + " \\rightarrow " + formulaToLaTeX(psi)
    case Not(phi) => "\\lnot " + formulaToLaTeX(phi)
  }
}
