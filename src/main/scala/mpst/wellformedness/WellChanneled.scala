package mpst.wellformedness

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*


object WellChanneled:
  private def isWellChanneled(global: Global): Boolean = global match
    case _: Interaction | _: RecursionCall | Skip => true
    case Sequence(globalA, globalB) => isWellChanneled(globalA) && isWellChanneled(globalB)
    case Parallel(globalA, globalB) => isWellChanneledAuxiliary(globalA, globalB) && isWellChanneled(globalA) && isWellChanneled(globalB)
    case Choice  (globalA, globalB) => isWellChanneled(globalA) && isWellChanneled(globalB)
    case RecursionFixedPoint(_, globalB) => isWellChanneled(globalB)
    case RecursionKleeneStar(globalA)    => isWellChanneled(globalA)
    case _ => throw new RuntimeException(s"found unexpected [$global]\n")
  end isWellChanneled

  private def isWellChanneledAuxiliary(globalA: Global, globalB: Global): Boolean =
    def communications(branch: Global): Set[(Participant, Participant, Label)] = branch match
      case interaction: Interaction => Set((interaction.sender, interaction.receiver, interaction.label))
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
  end isWellChanneledAuxiliary

  def apply(global:Global):Boolean =
    isWellChanneled(global)
  end apply
end WellChanneled