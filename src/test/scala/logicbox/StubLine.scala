package logicbox

import logicbox.framework.*
import logicbox.pl.*

case class StubLine(formula: PLFormula, rule: PropLogicRule, refs: List[Proof.Step[PLFormula, PropLogicRule]]) extends Proof.Line[PLFormula, PropLogicRule]
