package mpst.wellformedness

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

/* IDEA:
  - do not allow self-communication
    - in other words: do not allow p>p:m<s>

  problem: performance suffers from this naive solution and some works deal with it during Projection
*/

object WellCommunicated:
  private def isWellCommunicated(global:Global):Boolean =
    global match
      case Interaction(agentA,agentB,_,_) => agentA != agentB
      case RecursionCall(_) => true
      case Skip             => true
      case Sequence(globalA,globalB) => isWellCommunicated(globalA) && isWellCommunicated(globalB)
      case Parallel(globalA,globalB) => isWellCommunicated(globalA) && isWellCommunicated(globalB)
      case Choice  (globalA,globalB) => isWellCommunicated(globalA) && isWellCommunicated(globalB)
      case RecursionFixedPoint(_,globalB) => isWellCommunicated(globalB)
      case local => throw new RuntimeException(s"unexpected local type found in [$local]\n")
  end isWellCommunicated

  def apply(global:Global):Boolean =
    isWellCommunicated(global)
  end apply
end WellCommunicated