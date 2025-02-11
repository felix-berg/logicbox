package boxprover

import boxprover.PLFormula.And
import boxprover.PLFormula.Contradiction
import boxprover.PLFormula.Tautology
import boxprover.PLFormula.Atom
import boxprover.PLFormula.Or
import boxprover.PLFormula.Implies
import boxprover.PLFormula.Not

enum Side { case Left; case Right }

sealed abstract class Justification
case class Premise() extends Justification
case class Assumption() extends Justification
case class AndElim(which: Side, phi: PLFormula) extends Justification
case class ImpliesElim(why: PLFormula, implication: PLFormula) extends Justification
case class AndIntro(left: PLFormula, right: PLFormula) extends Justification
case class ImpliesIntro(box: SubProof) extends Justification

sealed abstract class ProofStep
case class Deduction(formula: PLFormula, just: Justification) extends ProofStep
case class SubProof(proof: Proof) extends ProofStep

type Proof = List[ProofStep]



object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    val lexer = PLLexer()
    val parser = PLParser()
    // val tokens = lexer.apply("(((p -> q) -> r) and (s -> not p) and (t) and (not s and t -> q)) -> r")
    // println(tokens)
    // val formula = parser.apply(tokens)
    // println(formula)
    // println(s"Formula is ${if (PLValidator.validate(formula)) then "valid" else "invalid"}")
    test(lexer, parser)
  }

  private def proofString(proof: Proof, map: Map[PLFormula | SubProof, String], depth: Int = 0): String = proof.map {
    case Deduction(formula, just) => 
      map(formula) ++ " "
        ++ (1 to depth).map { case _ => ' ' }.mkString 
        ++ s"$formula \t\t\t ${justificationString(just, map)}\n"

    case SubProof(proof) =>
       (1 to depth).map { case _ => ' ' }.mkString
        ++ s"------------ Box ------------\n"
        ++ proofString(proof, map, depth + 1)
        ++ s"-----------------------------\n"
  }.mkString

  private def printProof(proof: Proof, map: Map[PLFormula | SubProof, String]): Unit = 
    println(proofString(proof, map))

  def justificationString(just: Justification, names: Map[PLFormula | SubProof, String]): String = just match {
    case Premise() => "prem."
    case Assumption() => "ass."
    case AndElim(which, phi) => s"and-elim. ${names(phi)}"
    case ImpliesElim(why, implication) => s"impl.-elim. ${names(why)}, ${names(implication)}"
    case AndIntro(left, right) => s"and.-intr. ${names(left)}, ${names(right)}"
    case ImpliesIntro(box) => s"impl.-intr. ${names(box)}"
  }
  

  def validateProof(proof: Proof): Boolean = {
    val opt = proof.zip(proof.map {
      case Deduction(formula, just) => just match {
        case Premise() | Assumption() => true
        case AndElim(Side.Left, phi) => phi match {
          case And(lhs, _) => formula == lhs
          case _ => false
        }
        case AndElim(Side.Right, phi) => phi match {
          case And(_, rhs) => formula == rhs
          case _ => false
        }
        case ImpliesElim(why, implication) => implication match {
          case Implies(phi, psi) => 
            phi == why && formula == psi
          case _ => false
        }
        case AndIntro(left, right) => formula match {
          case And(l, r) => l == left && r == right
          case _ => false
        }
        case ImpliesIntro(box) => formula match {
          case Implies(phi, psi) => box match {
            case SubProof(Deduction(ass, Assumption()) :: steps) if steps.nonEmpty => 
              steps.last match {
                case Deduction(concl, _) => 
                  ass == phi && concl == psi
                case _ => false
              }
            case _ => false
          }
          case _ => false
        }
      }
      case SubProof(p) => validateProof(p)
    }).find { case (step, b) => !b }
    if (opt.isDefined) then println(s"failed step: ${opt.get}") else ()
    opt.isEmpty
  }

  def test(lexer: PLLexer, parser: PLParser): Unit = {
    val parse = (s: String) => parser.apply(lexer.apply(s))

    def ded(f: PLFormula, j: Justification) = Deduction(f, j)

    val s1 = ded(parse("p -> q"), Premise())
    val s2 = ded(parse("r -> s"), Premise())

    val ss1 = ded(parse("p and r"), Assumption())
    val ss2 = ded(parse("p"), AndElim(Side.Left, ss1.formula))
    val ss3 = ded(parse("r"), AndElim(Side.Right, ss1.formula))

    val ss4 = ded(parse("q"), ImpliesElim(ss2.formula, s1.formula))
    val ss5 = ded(parse("s"), ImpliesElim(ss3.formula, s2.formula))
    val ss6 = ded(parse("q and s"), AndIntro(ss4.formula, ss5.formula))

    val subproof = SubProof(List(ss1, ss2, ss3, ss4, ss5, ss6))
    val s3 = ded(parse("p and r -> q and s"), ImpliesIntro(subproof))

    val proof: Proof = List(
      s1, s2, subproof, s3
    )

    val map = Map[PLFormula | SubProof, String](
      s1.formula -> "1",
      s2.formula -> "2",
      subproof -> "3-8",
      ss1.formula -> "3",
      ss2.formula -> "4",
      ss3.formula -> "5",
      ss4.formula -> "6",
      ss5.formula -> "7",
      ss6.formula -> "8",
      s3.formula -> "9"
    )

    printProof(proof, map)
    println(s"\n\nProof is ${if(validateProof(proof)) then "valid" else "invalid"}")
  }
}
