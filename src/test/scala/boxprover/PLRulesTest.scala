package boxprover

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import org.scalatest.matchers.should.Matchers._
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

  describe("PropLogicRule::AndElim") {
    val leftRule = AndElim(Side.Left)

    it("should copy lhs of conjunction") {
      val ref = stub("p and (q or p and q -> r -> not not not (not p or r -> q))")
      val l = line("p", leftRule, List(ref))
      assert(leftRule.check(l.formula, List(ref)) ===  Nil)
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
}
