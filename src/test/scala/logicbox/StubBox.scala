package logicbox
import logicbox.framework.*
import logicbox.pl.*
case class StubBox(info: Unit, proof: Proof[PLFormula, PropLogicRule]) extends Proof.Box[PLFormula, PropLogicRule, Unit]
