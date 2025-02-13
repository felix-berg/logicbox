package boxprover

sealed abstract class PLFormula {
  import PLFormula.*

  override def toString(): String = this match {
    case Contradiction() => "⊥"
    case Tautology() => "⊤"
    case Atom(c) => c.toString
    case And(phi, psi) => s"($phi ∧ $psi)"
    case Or(phi, psi) => s"($phi ∨ $psi)"
    case Implies(phi, psi) => s"($phi -> $psi)"
    case Not(Atom(c)) => s"¬$c" // convenience
    case Not(phi) => s"¬($phi)"
  }
}

object PLFormula {
  case class Contradiction() extends PLFormula
  case class Tautology() extends PLFormula
  case class Atom(c: Char) extends PLFormula
  case class And(phi: PLFormula, psi: PLFormula) extends PLFormula
  case class Or(phi: PLFormula, psi: PLFormula) extends PLFormula
  case class Implies(phi: PLFormula, psi: PLFormula) extends PLFormula
  case class Not(phi: PLFormula) extends PLFormula
}

sealed abstract class PLToken

object PLToken {
  case class LeftParen() extends PLToken
  case class RightParen() extends PLToken
  case class And() extends PLToken
  case class Or() extends PLToken
  case class Implies() extends PLToken
  case class Not() extends PLToken
  case class Contradiction() extends PLToken
  case class Tautology() extends PLToken
  case class Atom(c: Char) extends PLToken
}
