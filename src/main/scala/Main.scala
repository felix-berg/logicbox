package boxprover

import java.lang.annotation.Annotation
import java.lang.reflect.Modifier
import java.lang.ref.Reference

object TestPropLogic {
  import boxprover.PLRules.*

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

  private def stub(formula: String): ProofStep[PLFormula] =
    ProofLine(
      formula = parse(formula),
      rule = new Rule[PLFormula] {
        def check(formula: PLFormula, refs: List[ProofStep[PLFormula]]): List[Mismatch] = Nil
      },
      refs = Nil
    )

  private def assertEq[T](a: T, b: T): Unit = {
    assert(a == b, s"$a != $b")
  }

  private def assertNeq[T](a: T, b: T): Unit = {
    assert(a != b, s"$a == $b")
  }

  // TODO: wrong num args for or and boxes
  def andElim(): Unit = {
    val leftRule = AndElim(Side.Left)
    {
      val ref = stub("p and (q or p and q -> r -> not not not (not p or r -> q))")
      val l = line("p", leftRule, List(ref))
      assertEq(leftRule.check(l.formula, List(ref)), Nil)
    }
    {
      // doesn't match rule
      val ref = stub("(p and q) or v")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      // wrong formula on lhs (is q, should be p)
      val ref = stub("q and (p -> v or r)")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }

    val rightRule = AndElim(Side.Left)
    {
      val ref = stub("p and (q or p and q -> r -> not not not (not p or r -> q))")
      val l = line("p", leftRule, List(ref))
      assertEq(leftRule.check(l.formula, List(ref)), Nil)
    }
    {
      // doesn't match rule
      val ref = stub("(p and q) or v")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      // wrong formula on lhs (is q, should be p)
      val ref = stub("q and (p -> v or r)")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }
  }

  def orIntro: Unit = {
    val leftRule = OrIntro(Side.Left)
    {
      val ref = stub("p")
      val l = line("p or (p -> q -> v)", leftRule, List(ref))
      assertEq(leftRule.check(l.formula, List(ref)), Nil)
    }
    {
      val ref = stub("p")
      val l = line("q or (p -> q -> v)", leftRule, List(ref))
      leftRule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      val ref = stub("p")
      val l = line("p and (p -> q -> v)", leftRule, List(ref))
      leftRule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case l => println(s"wow: $l")
      }
    }
    val rightRule = OrIntro(Side.Right)
    {
      val ref = stub("p")
      val l = line("(p -> q -> v) or p", rightRule, List(ref))
      assertEq(rightRule.check(l.formula, List(ref)), Nil)
    }
    {
      val ref = stub("p")
      val l = line("(p -> q -> v) or q", rightRule, List(ref))
      rightRule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReferences(List(0), _)) =>
        case l => println(s"wow: $l")
      }
    }
    {
      val ref = stub("p")
      val l = line("(p -> q -> v) and p", rightRule, List(ref))
      rightRule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
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
