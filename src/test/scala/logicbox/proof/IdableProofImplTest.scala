package logicbox.proof

import logicbox.framework.{Proof}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.*
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.Inspectors

import scala.collection.immutable.HashSet
import logicbox.framework.IdableProof

class IdableProofImplTest extends AnyFunSpec {
  case class StubFormula(i: Int)
  case class StubRule()

  describe("Proof") {
    it("should still be a proof") {
      val idproof: IdableProof[StubFormula, StubRule] = List(
        IdableProofImpl.Line("hello", StubFormula(1), StubRule(), Nil)
      )
      val proof: Proof[StubFormula, StubRule] = idproof
    }
  }

  describe("Line") {
    type L = Proof.Line[StubFormula, StubRule]

    val pl1: L = IdableProofImpl.Line("id1", StubFormula(1), StubRule(), Nil)
    val pl2: L = IdableProofImpl.Line("id1", StubFormula(1), StubRule(), Nil)
    val pl3: L = IdableProofImpl.Line("id1", StubFormula(2), StubRule(), Nil)
    val pl4: L = IdableProofImpl.Line("id2", StubFormula(1), StubRule(), Nil)
    val pl5: L = IdableProofImpl.Line("id2", StubFormula(2), StubRule(), Nil)

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

    it("toString should print the id") {
      pl1.toString should include ("id1")
      pl4.toString should include ("id2")
    }
  }
}
