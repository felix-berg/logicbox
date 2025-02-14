package boxprover


import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Inspectors
import scala.collection.immutable.HashSet

class ProofLineWithIdTest extends AnyFunSpec {
  case class StubFormula(i: Int)

  sealed trait StubRuleSet extends Rule[StubFormula] {
    case class Violation(expl: String = "THIS IS A STUB") extends ViolationTrait
    type RuleSet = StubRuleSet
  }

  case class StubRule(violations: List[String] = Nil) extends StubRuleSet {
    override def check(formula: StubFormula, refs: List[ProofStep[StubFormula, RuleSet]]): List[Violation] = 
      violations.map { s => Violation(expl = s) }
  }

  describe("ProofLineWithId") {
    type L = ProofLine[StubFormula, StubRuleSet]

    val pl1: L = ProofLineWithId(StubFormula(1), StubRule(), Nil, "id1")
    val pl2: L = ProofLineWithId(StubFormula(1), StubRule(), Nil, "id1")
    val pl3: L = ProofLineWithId(StubFormula(2), StubRule(), Nil, "id1")
    val pl4: L = ProofLineWithId(StubFormula(1), StubRule(), Nil, "id2")
    val pl5: L = ProofLineWithId(StubFormula(2), StubRule(), Nil, "id2")

    it("should respect equality when id's are equal (behave like base)") {
      assert(pl1 === pl2)
      assert(pl2 === pl1)
      assert(pl1 !== pl3)
      assert(pl3 !== pl1)

      assert(HashSet(pl1, pl2).size == 1)
      assert(HashSet(pl2, pl1).size == 1)
      assert(HashSet(pl1, pl3).size == 2)
      assert(HashSet(pl3, pl1).size == 2)

      assert(Set(pl1, pl2).size == 1)
      assert(Set(pl2, pl1).size == 1)
      assert(Set(pl1, pl3).size == 2)
      assert(Set(pl3, pl1).size == 2)
    }

    it("should not be equal when ids mismatch") {
      assert(pl1 !== pl4)
      assert(pl4 !== pl1)
      assert(pl1 !== pl5)
      assert(pl5 !== pl1)

      assert(Set(pl1, pl4).size == 2)
      assert(Set(pl4, pl1).size == 2)
      assert(Set(pl1, pl5).size == 2)
      assert(Set(pl5, pl1).size == 2)

      assert(HashSet(pl1, pl4).size == 2)
      assert(HashSet(pl4, pl1).size == 2)
      assert(HashSet(pl1, pl5).size == 2)
      assert(HashSet(pl5, pl1).size == 2)
    }
  }
}
