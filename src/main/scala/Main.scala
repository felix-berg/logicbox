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
        case List(FormulaDoesntMatchReference(0, _)) =>
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
        case List(FormulaDoesntMatchReference(0, _)) =>
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
        case List(FormulaDoesntMatchReference(0, _)) =>
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
        case List(FormulaDoesntMatchReference(0, _)) =>
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

  def orElim: Unit = {
    val rule = OrElim()

    {
      // r0 is not line
      val (r0, r1, r2) = (boxStub("p or q", "s"), boxStub("p", "s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeLine(0, _)) =>
        case l => println(s"wow: $l")
      }
    }

    {
      // r1 is not box
      val (r0, r1, r2) = (stub("p or q"), stub("s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(1, _)) =>
        case l => println(s"wow: $l")
      }
    }

    {
      // r2 is not box
      val (r0, r1, r2) = (stub("p or q"), boxStub("p", "s"), stub("q"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(2, _)) =>
        case l => println(s"wow: $l")
      }
    }
  
    {
      val (r0, r1, r2) = (stub("p -> q"), boxStub("p", "s"), boxStub("q", "s"))
      val l = line("s", rule, List(r0, r1, r2))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case l => println(s"wow: $l")
      }
    }

    {
      val refs = List(stub("p or q"), boxStub("p", "s"), boxStub("q", "d"))
      val l = line("s", rule, refs)
      val mms = rule.check(l.formula, l.refs)
      val good = mms.exists {
        case ReferencesMismatch(List(1, 2), _) => true
        case _ => false
      } && mms.exists {
        case FormulaDoesntMatchReference(2, _) => true
        case _ => false
      }
      if (!good) println(s"wow: $mms")
    }

    {
      val refs = List(stub("p or q"), boxStub("p", "d"), boxStub("q", "s"))
      val l = line("s", rule, refs)
      val mms = rule.check(l.formula, l.refs)
      val good = mms.exists {
        case ReferencesMismatch(List(1, 2), _) => true
        case _ => false
      } && mms.exists {
        case FormulaDoesntMatchReference(1, _) => true
        case _ => false
      }
      if (!good) println(s"wow: $mms")
    }
  }

  def andIntro: Unit = {
    val rule = AndIntro()
    val List(r0, r1) = List("p", "q").map(stub)
    {
      val l = line("p and q", rule, List(r0, r1))
      assertEq(rule.check(l.formula, l.refs), Nil)
    }

    {
      val l = line("r and (s or v)", rule, List(r0, r1))
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

    {
      val l = line("r and q", rule, List(r0, r1))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) => 
        case s => print(s"wow: $s")
      }
    }

    {
      val l = line("p and r", rule, List(r0, r1))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(1, _)) => 
        case s => print(s"wow: $s")
      }
    }
  }

  def extractAssumptionConclusionTest = {
    val emptybox = ProofBox(info = (), proof = (Nil: List[ProofStep[PLFormula]]))
    {
      val box = ProofBox(info = (), proof = (Nil: List[ProofStep[PLFormula]]))
      PropLogicRule.extractAssumptionConclusion(box) match {
        case Right(List(MiscellaneousMismatch(_))) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val assmp = ProofLine(parse("p"), Premise(), Nil) // not assumption
      val concl = stub("q")
      val box = ProofBox(info = (), proof = List(assmp, concl))
      PropLogicRule.extractAssumptionConclusion(box) match {
        case Right(List(MiscellaneousMismatch(_))) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val box = ProofBox(info = (), proof = List(
        emptybox,
        stub("q")
      ))
      PropLogicRule.extractAssumptionConclusion(box) match {
        case Right(List(MiscellaneousMismatch(_))) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val box = ProofBox(info = (), proof = List(
        ProofLine(parse("p"), Assumption(), Nil),
        emptybox
      ))
      PropLogicRule.extractAssumptionConclusion(box) match {
        case p @ Right(List(MiscellaneousMismatch(_))) => 
        case s => println(s"huh: $s")
      }
    }
  }

  def implicationIntro = {
    val rule = ImplicationIntro()
    {
      val box = boxStub("p", "q")
      val l = line("r -> q", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("p", "q")
      val l = line("p -> r", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("p", "q")
      val l = line("p and q", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val l = line("p -> q", rule, List(stub("q")))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceShouldBeBox(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def implicationElim = {
    val rule = ImplicationElim()
    {
      val (r0, r1) = (stub("p"), stub("r -> q"))
      val l = line("q", rule, List(r0, r1))
      rule.check(l.formula, l.refs) match {
        case List(ReferencesMismatch(List(0, 1), _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val (r0, r1) = (stub("p"), stub("p -> q"))
      val l = line("r", rule, List(r0, r1))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(1, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val (r0, r1) = (stub("p"), stub("p and q"))
      val l = line("q", rule, List(r0, r1))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(1, _)) => 
        case s => println(s"huh: $s")
      }
    }
  }

  def notIntro = {
    val rule = NotIntro()
    {
      val box = boxStub("p", "q")
      val l = line("not p", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("p", "false")
      val l = line("not q", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val box = boxStub("p", "false")
      val l = line("p", rule, List(box))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def notElim = {
    val rule = NotElim()
    {
      val refs = List(stub("p"), stub("not q"))
      val l = line("false", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(ReferencesMismatch(List(0, 1), _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p"), stub("p"))
      val l = line("false", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(1, _)) =>
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p"), stub("not p"))
      val l = line("p", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def contradictionElim = {
    val rule = ContradictionElim()
    {
      val ref = stub("p")
      val l = line("p", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) =>
        case s => println(s"huh: $s")
      }
    }
  }

  def notNotElim = {
    val rule = NotNotElim() 
    {
      val ref = stub("not p")
      val l = line("p", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val ref = stub("not not p")
      val l = line("q", rule, List(ref))
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) => 
        case s => println(s"huh: $s")
      }
    }
  }

  def modusTollens = {
    val rule = ModusTollens()
    {
      val refs = List(stub("p -> q"), stub("not q"))
      val l = line("p", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchRule(_)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p and q"), stub("not q"))
      val l = line("not p", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(0, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p -> q"), stub("q"))
      val l = line("not p", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(ReferenceDoesntMatchRule(1, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p -> q"), stub("not q"))
      val l = line("not r", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(FormulaDoesntMatchReference(0, _)) => 
        case s => println(s"huh: $s")
      }
    }
    {
      val refs = List(stub("p -> q"), stub("not r"))
      val l = line("not p", rule, refs)
      rule.check(l.formula, l.refs) match {
        case List(ReferencesMismatch(List(0, 1), _)) => 
        case s => println(s"huh: $s")
      }
    }
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
