package mpst.utility

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import scala.annotation.tailrec


object StructuralCongruence:
  private def runOnce(protocol:Protocol): Protocol =
    protocol match
      // terminal cases //
      case _: Interaction | _: Send | _: Recv | _: RecursionCall | Skip => protocol
      // recursive: associate ";", "||" and "+" //
      case Sequence(Sequence(protocolA, protocolB), protocolC) => runOnce(Sequence(protocolA, Sequence(protocolB, protocolC)))
      case Parallel(Parallel(protocolA, protocolB), protocolC) => runOnce(Parallel(protocolA, Parallel(protocolB, protocolC)))
      case Choice  (Choice  (protocolA, protocolB), protocolC) => runOnce(Choice  (protocolA, Choice  (protocolB, protocolC)))
      // recursive: skip //
      case Sequence(protocolA, Skip) => runOnce(protocolA) // @ telmo - can I count this as Struct.Congruence?
      case Sequence(Skip, protocolB) => runOnce(protocolB) // @ telmo - can I count this as Struct.Congruence?
      case Parallel(protocolA, Skip) => runOnce(protocolA) // @ telmo - usually literature allows this on "0"
      case Parallel(Skip, protocolB) => runOnce(protocolB) // @ telmo - usually literature allows this on "0"
      case Choice  (protocolA, Skip) => runOnce(protocolA) // @ telmo - experimenting
      case Choice  (Skip, protocolB) => runOnce(protocolB) // @ telmo - experimenting
      // @ telmo - CHOICE IS DANGEROUS!
      case RecursionFixedPoint(_, Skip) => Skip            // @ telmo - usually literature allows this on "0"
      case RecursionKleeneStar(Skip)    => Skip
      /* @ telmo -
        choreo allows
        Choice(c1,c1) => runOnce(c1)
        does this count as structural congruence? No but it fits a full merge rule
      */
      // recursive: default //
      case Sequence(protocolA, protocolB) => Sequence(runOnce(protocolA), runOnce(protocolB))
      case Parallel(protocolA, protocolB) => Parallel(runOnce(protocolA), runOnce(protocolB))
      case Choice  (protocolA, protocolB) => Choice  (runOnce(protocolA), runOnce(protocolB))
      case RecursionFixedPoint(variable, protocolB) => RecursionFixedPoint(variable, runOnce(protocolB))
      case RecursionKleeneStar(protocolA)           => RecursionKleeneStar(runOnce(protocolA))
  end runOnce

  @tailrec
  private def run(protocol: Protocol): Protocol =
    val congruentProtocol = runOnce(protocol)
    if  congruentProtocol == protocol
    then protocol
    else run(congruentProtocol)
  end run

  def apply(protocol:Protocol):Protocol = run(protocol)
end StructuralCongruence