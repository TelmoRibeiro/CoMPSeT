package mpst.wellformedness

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

/* IDEA:
  - check well-formedness (linearity?) on *parallel* branches
    - channel = sendingAgent -> receivingAgent
    - if channels are shared in both branches then *error*
    - otherwise communications are secure since they are independent
    - error = "possible dependent communications in branch [globalA] and [globalB] caused by the shared channels [sharedChannels]"

    problem:
      can we not relax this condition?
      there is no cases where shared channels are not risky?
      not even trying to disambiguate by message and sort that pass in the channel
      extend to the other definition (Honda et al)
*/

object WellChannelled:
  private def wellChannelled(global:Global): Boolean = global match
    case _: Interaction | _: RecursionCall | Skip => true
    case Sequence(globalA, globalB) => wellChannelled(globalA) && wellChannelled(globalB)
    case Parallel(globalA,globalB) =>
      val sharedChannels = channels(globalA).intersect(channels(globalB))
      if sharedChannels.nonEmpty then throw RuntimeException(s"possible dependent communications in branch [$globalA] and [$globalB] caused by the shared channels [$sharedChannels]")
      sharedChannels.isEmpty
    case Choice(globalA,globalB) => wellChannelled(globalA) && wellChannelled(globalB)
    case RecursionFixedPoint(_, globalB) => wellChannelled(globalB)
    case RecursionKleeneStar(globalA)    => wellChannelled(globalA)
    case _ => throw new RuntimeException(s"found unexpected [$global]\n")
  end wellChannelled

  private def channels(branch: Global): Set[(Participant, Participant)] = branch match
    case Interaction(sender, receiver, _, _) => Set.empty + (sender -> receiver)
    case _: RecursionCall | Skip => Set.empty
    case Sequence(globalA, globalB) => channels(globalA) ++ channels(globalB)
    case Parallel(globalA, globalB) => channels(globalA) ++ channels(globalB)
    case Choice  (globalA, globalB) => channels(globalA) ++ channels(globalB)
    case RecursionFixedPoint(_,globalB) => channels(globalB)
    case RecursionKleeneStar(globalA)   => channels(globalA)
    case _ => throw new RuntimeException(s"found unexpected [$branch]")
  end channels

  def apply(global:Global):Boolean =
    wellChannelled(global)
  end apply
end WellChannelled