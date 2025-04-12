package mpst.syntax

import mpst.syntax.Protocol.*

import scala.annotation.tailrec


object Simplifier:
  private def runOnce(protocol:Protocol): Protocol = protocol match
    case _: Interaction | _: Action | _: RecursionCall | Skip => protocol
    case Sequence(Sequence(protocolA, protocolB), protocolC) => runOnce(Sequence(protocolA, Sequence(protocolB, protocolC)))
    case Parallel(Parallel(protocolA, protocolB), protocolC) => runOnce(Parallel(protocolA, Parallel(protocolB, protocolC)))
    case Choice  (Choice  (protocolA, protocolB), protocolC) => runOnce(Choice  (protocolA, Choice  (protocolB, protocolC)))
    case Sequence(protocolA, Skip) => runOnce(protocolA)
    case Sequence(Skip, protocolB) => runOnce(protocolB)
    case Parallel(protocolA, Skip) => runOnce(protocolA)
    case Parallel(Skip, protocolB) => runOnce(protocolB)
    //case Choice  (protocolA, Skip) => runOnce(protocolA)
    //case Choice  (Skip, protocolB) => runOnce(protocolB)
    // @ telmo - a choice between doing something or skipping should not be considered the same as just doing something
    case RecursionFixedPoint(_, Skip) => Skip // @ telmo - took from struct. congruence
    case RecursionKleeneStar(Skip)    => Skip // @ telmo - abusing from the previous notation
    case Choice(protocolA, protocolB) if protocolA == protocolB => runOnce(protocolA) // @ telmo - a choice between doing A or doing A is no choice at all
    case Sequence(protocolA, protocolB) => Sequence(runOnce(protocolA), runOnce(protocolB))
    case Parallel(protocolA, protocolB) => Parallel(runOnce(protocolA), runOnce(protocolB))
    case Choice  (protocolA, protocolB) => Choice  (runOnce(protocolA), runOnce(protocolB))
    case RecursionFixedPoint(variable, protocolB) => RecursionFixedPoint(variable, runOnce(protocolB))
    case RecursionKleeneStar(protocolA)           => RecursionKleeneStar(runOnce(protocolA))
  end runOnce

  @tailrec
  private def run(protocol: Protocol): Protocol =
    val simplifiedProtocol = runOnce(protocol)
    if simplifiedProtocol == protocol then protocol else run(simplifiedProtocol)
  end run

  def apply(protocol: Protocol): Protocol =
    run(protocol)
  end apply
end Simplifier