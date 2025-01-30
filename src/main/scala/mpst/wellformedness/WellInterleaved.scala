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
      None?
*/

object WellInterleaved:
  private def isWellInterleaved(global: Global): Boolean = global match
    case _: Interaction | _: RecursionCall | Skip => true
    case Sequence(globalA, globalB) => isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case Parallel(globalA, globalB) => isWellInterleavedAuxiliary(globalA, globalB) && isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case Choice  (globalA, globalB) => isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case RecursionFixedPoint(_, globalB) => isWellInterleaved(globalB)
    case RecursionKleeneStar(globalA)    => isWellInterleaved(globalA)
    case _ => throw new RuntimeException(s"found unexpected [$global]\n")
  end isWellInterleaved

  private def isWellInterleavedAuxiliary(globalA: Global, globalB: Global): Boolean =
    def communications(branch: Global): Set[(Participant, Participant, Label)] = branch match
      case Interaction(sender, receiver, label, _) => Set((sender, receiver, label))
      case _: RecursionCall | Skip => Set.empty
      case Sequence(globalA, globalB) => communications(globalA) ++ communications(globalB)
      case Parallel(globalA, globalB) => communications(globalA) ++ communications(globalB)
      case Choice  (globalA, globalB) => communications(globalA) ++ communications(globalB)
      case RecursionFixedPoint(_,globalB) => communications(globalB)
      case RecursionKleeneStar(globalA)   => communications(globalA)
      case _ => throw new RuntimeException(s"found unexpected [$branch]")
    end communications

    val sharedCommunications = communications(globalA).intersect(communications(globalB))
    if  sharedCommunications.nonEmpty
    then throw RuntimeException(s"possible dependent communications in branch [$globalA] and [$globalB] caused by the shared communications [$sharedCommunications]")
    else true
  end isWellInterleavedAuxiliary

  def apply(global:Global):Boolean =
    isWellInterleaved(global)
  end apply
end WellInterleaved