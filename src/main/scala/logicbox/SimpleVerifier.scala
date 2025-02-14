package logicbox

import framework._

case class SimpleVerifier[F, R, V](checker: RuleChecker[F, R, V]) extends Verifier[F, R, V] {
  def verify(proof: Proof[F, R]): List[VerifierResult[F, R, V]] =
    proof.flatMap {
      case line @ Proof.Line[F, R](formula, rule, refs) => 
        checker.check(rule, formula, refs).map(violation => VerifierResult(line, violation))
      case Proof.Box(_, proof: Proof[F, R] @unchecked) => verify(proof)
      case _ => 
        throw NotImplementedError("unreachable case, I thought (proof step is either line/box)???")
    }
}
