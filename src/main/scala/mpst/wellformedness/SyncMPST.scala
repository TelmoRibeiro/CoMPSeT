package mpst.wellformedness

import mpst.syntax.Protocol.*

object SyncMPST:
  private def isSyncMPST(global:Global):Boolean =
    global match
      case Interaction(_,_,_,_) => true
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(globalA,globalB) => isSyncMPST(globalA) && isSyncMPST(globalB)
      case Parallel(globalA,globalB) =>
        val agentsA = getParticipants(globalA)
        val agentsB = getParticipants(globalB)
        agentsA.intersect(agentsB).isEmpty
      case Choice(globalA,globalB) => isSyncMPST(globalA) && isSyncMPST(globalB)
      case RecursionFixedPoint(_,globalB) => isSyncMPST(globalB)
      case local => throw new RuntimeException(s"unexpected local type found in [$local]\n")
  end isSyncMPST

  def apply(global:Global):Boolean =
    isSyncMPST(global)
  end apply
end SyncMPST