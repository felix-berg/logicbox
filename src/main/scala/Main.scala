package boxprover

import java.lang.annotation.Annotation
import java.lang.reflect.Modifier
import java.lang.ref.Reference

trait Formula {
  def toASCII: String
  def toLaTeX: String
}

sealed abstract class Mismatch {
  def expl: String
}

case class WrongNumberOfReferences(exp: Int, actual: Int, expl: String = "") extends Mismatch
case class ReferenceShouldBeBox(ref: Int, expl: String = "") extends Mismatch
case class ReferenceShouldBeLine(ref: Int, expl: String = "") extends Mismatch
case class ReferenceDoesntMatchRule(actual: Int, expected: Formula, expl: String = "") extends Mismatch
case class ReferencesMismatch(refs: List[Int], expl: String = "") extends Mismatch
case class FormulaDoesntMatchReferences(refs: List[Int], expl: String = "") extends Mismatch

trait Rule[F] {
  def check(formula: F, refs: List[ProofStep[F]]): List[Mismatch]
}

sealed abstract class ProofStep[+Formula]
case class ProofLine[F](formula: F, rule: Rule[F], refs: List[ProofStep[F]]) extends ProofStep[F]
case class ProofBox[F, I](boxinfo: I, proof: Proof[F]) extends ProofStep[F]

type Proof[+F] = List[ProofStep[F]]

object PropLogic {
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

    if (good) Left(ls.map {
      case Left(f) => f
      case _ => ???
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
    private def phiAndPsi: Formula = new Formula {
      override def toASCII: String = "φ -> ψ"
      override def toLaTeX: String = "\\phi \\rightarrow \\psi"
    }

    private def checkMatchesRef(formula: PLFormula, ref: PLFormula): List[Mismatch] = ref match {
      case And(lhs, rhs) => side match {
        case Side.Left => 
          if (lhs != formula) List(
            FormulaDoesntMatchReferences(List(0), "formula doesn't match left-hand side")
          ) else Nil
            
        case Side.Right =>
          if (rhs != formula) List(
            FormulaDoesntMatchReferences(List(0), "formula doesn't match right-hand side")
          ) else Nil
      }
      
      case _ => List(
        ReferenceDoesntMatchRule(0, phiAndPsi, "should be conjuction (and)")
      )
    }

    def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = {
      checkCorrectNumberOfRefs(refs, 1) ++ {
        (extractFormulas(refs): @unchecked) match {
          case Left(ref :: _) =>
            checkMatchesRef(formula, ref)
          case Right(mismatches) => 
            mismatches
        }
      }
    }
  }
}

object TestPropLogic {
  import PropLogic.*

  private val lexer = PLLexer()
  private val parser = PLParser()
  private def parse(str: String): PLFormula = parser(lexer(str))

  private def line(formula: String, rule: Rule[PLFormula], refs: List[ProofStep[PLFormula]]): ProofLine[PLFormula] = {
    ProofLine(
      formula = parse(formula),
      rule = rule,
      refs = refs
    )
  }

  private def assertEq[T](a: T, b: T): Unit = {
    assert(a == b, s"$a != $b")
  }

  private def assertNeq[T](a: T, b: T): Unit = {
    assert(a != b, s"$a == $b")
  }

  def andElim(): Unit = {
    val leftRule = AndElim(Side.Left)
    val rightRule = AndElim(Side.Left)
    // TODO: wrong num args
    {
      val ref = line("p and (q or p and q -> r -> not not not (not p or r -> q))", null, Nil)
      val l = line("p", leftRule, List(ref))
      assertEq(leftRule.check(l.formula, List(ref)), Nil)
    }
    {
      // doesn't match rule
      val ref = line("(p and q) or v", null, Nil)
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(ReferenceDoesntMatchRule(0, _, _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      // wrong formula on lhs (is q, should be p)
      val ref = line("q and (p -> v or r)", null, Nil)
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }

    {
      val ref = line("p and (q or p and q -> r -> not not not (not p or r -> q))", null, Nil)
      val l = line("p", leftRule, List(ref))
      assertEq(leftRule.check(l.formula, List(ref)), Nil)
    }
    {
      // doesn't match rule
      val ref = line("(p and q) or v", null, Nil)
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(ReferenceDoesntMatchRule(0, _, _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      // wrong formula on lhs (is q, should be p)
      val ref = line("q and (p -> v or r)", null, Nil)
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }
  }
}

object Main extends PLParser {
  def main(args: Array[String]): Unit = {
    val lexer = PLLexer()
    val parser = PLParser()
    val tokens = lexer.apply("(((p -> q) -> r) and (s -> not p) and (t) and (not s and t -> q)) -> r")
    val formula = parser.apply(tokens)

    val ignored = Set("getClass", "toString", "notify", "notifyAll", "wait", "hashCode")

    TestPropLogic.getClass.getMethods
      .filter(m => Modifier.isPublic(m.getModifiers))
      .filter(m => m.getParameterCount() == 0)
      .filter(m => !ignored.contains(m.getName))
      .foreach(m => {
        println(s"Running test: ${m.getName}")
        m.invoke(TestPropLogic)
      })
  }
}
