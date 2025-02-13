package boxprover

import java.lang.annotation.Annotation
import java.lang.reflect.Modifier
import java.lang.ref.Reference

object TestPropLogic {
  import boxprover.PropLogicRule.*

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

  private def assertEq[T](a: T, b: T): Unit = {
    assert(a == b, s"$a != $b")
  }

  private def assertNeq[T](a: T, b: T): Unit = {
    assert(a != b, s"$a == $b")
  }

  def notNotIntro = {
    val rule = NotNotIntro()
    {
      val ref = stub("q")
      val l = line("q", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val ref = stub("q")
      val l = line("not not p", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def proofByContradiction = {
    val rule = ProofByContradiction()
    {
      val ref = stub("false") // not a box
      val l = line("q", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(0, _)) => 
        case _ => Nil
      }
    }
    {
      val box = boxStub("p", "false")
      val l = line("p", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("not p", "true") // should end in bot
      val l = line("p", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("not p", "false")
      val l = line("q", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def lawOfExcludedMiddle = {
    val rule = LawOfExcludedMiddle()
    {
      val l = line("p or p", rule, Nil)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val l = line("p or not q", rule, Nil)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def copy = {
    val rule = Copy()
    {
      val ref = stub("q")
      val l = line("p", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def fullProof = {
    val l1 = line("p -> q", Premise(), Nil)
    val l2 = line("r -> s", Premise(), Nil)

    val l3 = line("p and r", Assumption(), Nil)
    val l4 = line("p", AndElim(Side.Left), List(l3))
    val l5 = line("r", AndElim(Side.Right), List(l3))
    val l6 = line("q", ImplicationElim(), List(l4, l1))
    val l7 = line("s", ImplicationElim(), List(l5, l2))
    val l8 = line("q and s", AndIntro(), List(l6, l7))

    val box = ProofBox(info = (), proof = List(l3, l4, l5, l6, l7, l8))
    val l9 = line("p and r -> q and s", ImplicationIntro(), List(box))

    def checkProof(p: Proof[PLFormula]): List[(PLFormula, Mismatch)] = p.flatMap {
      case ProofLine(formula, rule, refs) => rule.check(formula, refs).map((formula, _))
      case ProofBox(_, proof) => checkProof(proof)
    }

    val proof = List(l1, l2, box, l9)
    checkProof(proof).foreach {
      case (formula, mismatch) => println(s"$formula:\n $mismatch")
    }
  }

  def bigProof = {
    val l1  = line("(p -> q) -> r", Premise(), Nil)
    val l2  = line("s -> not p", Premise(), Nil)
    val l3  = line("t", Premise(), Nil)
    val l4  = line("(not s and t) -> q", Premise(), Nil)
    val l5  = line("p or not p", LawOfExcludedMiddle(), Nil)

    val l6  = line("p", Assumption(), Nil)
    val l7  = line("not not p", NotNotIntro(), List(l6))
    val l8  = line("not s", ModusTollens(), List(l2, l7))
    val l9  = line("not s and t", AndIntro(), List(l8, l3))
    val l10 = line("q", ImplicationElim(), List(l9, l4))

    val l11 = line("p", Assumption(), Nil)
    val l12 = line("q", Copy(), List(l10))
    val b1 = ProofBox(info = (), List(l11, l12))

    val l13 = line("p -> q", ImplicationIntro(), List(b1))
    val b2 = ProofBox(info = (), List(l6, l7, l8, l9, l10, b1, l13))

    val l14 = line("not p", Assumption(), Nil)
    val l15 = line("p", Assumption(), Nil)
    val l16 = line("false", NotElim(), List(l15, l14))
    val l17 = line("q", ContradictionElim(), List(l16))
    val b3 = ProofBox(info = (), List(l15, l16, l17))

    val l18 = line("p -> q", ImplicationIntro(), List(b3))
    val b4 = ProofBox(info = (), List(l14, b3, l18))

    val l19 = line("p -> q", OrElim(), List(l5, b2, b4))
    val l20 = line("r", ImplicationElim(), List(l19, l1))

    def checkProof(p: Proof[PLFormula]): List[(PLFormula, Mismatch)] = p.flatMap {
      case ProofLine(formula, rule, refs) => rule.check(formula, refs).map((formula, _))
      case ProofBox(_, proof) => checkProof(proof)
    }

    val proof = List(l1, l2, l3, l4, l5, b2, b4, l19, l20)
    checkProof(proof).foreach {
      case (formula, mismatch) => println(s"$formula:\n $mismatch")
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
