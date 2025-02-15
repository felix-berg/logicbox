package logicbox.proof
import logicbox.framework.*
import logicbox.formula.*
case class StubBox(info: Unit, proof: Proof[PLFormula, PropLogicRule]) extends Proof.Box[PLFormula, PropLogicRule, Unit]
