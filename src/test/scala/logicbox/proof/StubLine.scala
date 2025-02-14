package logicbox.proof

import logicbox.framework.*
import logicbox.formula.*

case class StubLine(formula: PLFormula, rule: PropLogicRule, refs: List[Proof.Step[PLFormula, PropLogicRule]]) extends Proof.Line[PLFormula, PropLogicRule]
