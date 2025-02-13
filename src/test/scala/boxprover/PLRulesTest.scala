package boxprover

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Inspectors
import java.lang.ref.Reference

class PLRulesTest extends AnyFunSpec {
  import PropLogicRule._

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

  private def boxStub(ass: String, concl: String): ProofBox[PLFormula, Unit] = ProofBox(
    info = (), proof = List(
      ProofLine(parse(ass), Assumption(), Nil),
      stub(concl)
    )
  )

  describe("AndElim") {
    val leftRule = AndElim(Side.Left)

    it("should copy lhs of conjunction") {
      val ref = stub("p and (q or p and q -> r -> not not not (not p or r -> q))")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) should be (Nil)
    }

    it("should reject disjunction (doesn't match rule, left)") {
      val ref = stub("(p and q) or v")
      val l = line("p and q", leftRule, List(ref))

      leftRule.check(l.formula, List(ref)) should matchPattern {
        case List(ReferenceDoesntMatchRule(0, _)) => 
      }
    }

    it("should reject disjunction (right)"){
      // wrong formula on lhs (is q, should be p)
      val ref = stub("q and (p -> v or r)")
      val l = line("p", leftRule, List(ref))
      leftRule.check(l.formula, List(ref)) should matchPattern {
        case List(FormulaDoesntMatchReference(0, _)) =>
      }
    }

    val rightRule = AndElim(Side.Right)
    it("should elim rhs") {
      val ref = stub("p and (q or p and q -> r -> not not not (not p or r -> q))")
      val l = line("(q or p and q -> r -> not not not (not p or r -> q))", rightRule, List(ref))
      assert(rightRule.check(l.formula, List(ref)) === Nil)
    }
    it("should not match or") {
      // doesn't match rule
      val ref = stub("(p and q) or v")
      val l = line("p", rightRule, List(ref))
      rightRule.check(l.formula, List(ref)) should matchPattern {
        case List(ReferenceDoesntMatchRule(0, _)) =>
      }
    }
    it("should not match wrong atom") {
      // wrong formula on lhs (is q, should be p)
      val ref = stub("q and (p -> v or r)")
      val l = line("p", rightRule, List(ref))
      rightRule.check(l.formula, List(ref)) should matchPattern {
        case List(FormulaDoesntMatchReference(0, _)) =>
      }
    }
  }

  describe("orIntro") {
    val leftRule = OrIntro(Side.Left)
    it("should intro with reference on lhs") {
      val ref = stub("p")
      val l = line("p or (p -> q -> v)", leftRule, List(ref))
      assert(leftRule.check(l.formula, List(ref)) === Nil)
    }
    it("should not match with wrong lhs") {
      val ref = stub("p")
      val l = line("q or (p -> q -> v)", leftRule, List(ref))
      leftRule.check(l.formula, l.refs) should matchPattern {
        case List(FormulaDoesntMatchReference(0, _)) =>
      }
    }
    it("should not introduce and (left)") {
      val ref = stub("p")
      val l = line("p and (p -> q -> v)", leftRule, List(ref))
      leftRule.check(l.formula, l.refs) should matchPattern {
        case List(FormulaDoesntMatchRule(_)) =>
      }
    }
    val rightRule = OrIntro(Side.Right)
    it("should introduce with ref on rhs") {
      val ref = stub("p")
      val l = line("(p -> q -> v) or p", rightRule, List(ref))
      assert(rightRule.check(l.formula, List(ref)) === Nil)
    }
    it("should not introduce q when ref is p") {
      val ref = stub("p")
      val l = line("(p -> q -> v) or q", rightRule, List(ref))
      rightRule.check(l.formula, l.refs) should matchPattern {
        case List(FormulaDoesntMatchReference(0, _)) =>
      }
    }
    it("should not introduce and (right)") {
      val ref = stub("p")
      val l = line("(p -> q -> v) and p", rightRule, List(ref))
      rightRule.check(l.formula, l.refs) should matchPattern {
        case List(FormulaDoesntMatchRule(_)) =>
      }
    }
  }

  describe("OrElim") {
    val rule = OrElim()
    it("should reject when first ref is box") {
      // r0 is not line
      val (r0, r1, r2) = (boxStub("p or q", "s"), boxStub("p", "s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeLine(0, _)) =>
        case l => println(s"wow: $l")
      }
    }

    it("should reject when second ref is line") {
      // r1 is not box
      val (r0, r1, r2) = (stub("p or q"), stub("s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(1, _)) =>
        case l => println(s"wow: $l")
      }
    }

    it("should reject when third ref is line"){
      // r2 is not box
      val (r0, r1, r2) = (stub("p or q"), boxStub("p", "s"), stub("q"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(2, _)) =>
        case l => println(s"wow: $l")
      }
    }
  
    it("should reject implication (should be or)") {
      val (r0, r1, r2) = (stub("p -> q"), boxStub("p", "s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case l => println(s"wow: $l")
      }
    }

    it("should reject incorrect conclusion (third ref)") {
      val refs = List(stub("p or q"), boxStub("p", "s"), boxStub("q", "d"))
      val l = line("s", rule, refs)
      val mms = rule.check(l.formula, l.refs)
      Inspectors.forAtLeast(1, mms) {
        _ should matchPattern {
          case ReferencesMismatch(List(1, 2), _) =>
        }
      }
      Inspectors.forAtLeast(1, mms) {
        _ should matchPattern {
          case FormulaDoesntMatchReference(2, _) =>
        }
      }
    }

    it("should reject incorrect conclusion (second ref)") {
      val refs = List(stub("p or q"), boxStub("p", "d"), boxStub("q", "s"))
      val l = line("s", rule, refs)
      val mms = rule.check(l.formula, l.refs)
      Inspectors.forAtLeast(1, mms) {
        _ should matchPattern {
          case ReferencesMismatch(List(1, 2), _) =>
        }
      }
      Inspectors.forAtLeast(1, mms) {
        _ should matchPattern {
          case FormulaDoesntMatchReference(1, _) =>
        }
      }
    }
  }

  describe("AndIntro") {
    val rule = AndIntro()
    val refs = List("p", "q").map(stub)

    it("should work with correct usage") {
      val l = line("p and q", rule, refs)
      rule.check(l.formula, l.refs) should be (Nil)
    }

    it("should report two formula mismatches when both operands are wrong") {
      val l = line("r and (s or v)", rule, refs)
      val mismatches = rule.check(l.formula, l.refs)
      mismatches.exists {
        case FormulaDoesntMatchReference(0, _) => true
        case _ => false
      } 
      mismatches.exists {
        case FormulaDoesntMatchReference(1, _) => true
        case _ => false
      }
    }

    it("should report lhs mismatches ref") {
      val l = line("r and q", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) => 
        case s => print(s"wow: $s")
      }
    }

    it("should report rhs mismatches ref") {
      val l = line("p and r", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(1, _)) => 
        case s => print(s"wow: $s")
      }
    }
  }
}
