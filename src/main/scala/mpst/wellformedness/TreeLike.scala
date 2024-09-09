package mpst.wellformedness

import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

object TreeLike:
  private def isTreeLike(global:Global):Boolean =
    global match
      case Interaction(_,_,_,_) => true
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(globalA,globalB) =>
        globalA match
          case Choice(gA,gB) if globalB == Skip => isTreeLike(gA) && isTreeLike(gB)
          case _ => isTreeLike(globalA,globalB)
      case Parallel(globalA,globalB) => isTreeLike(globalA) && isTreeLike(globalB)
      case Choice  (globalA,globalB) => isTreeLike(globalA) && isTreeLike(globalB)
      case RecursionFixedPoint(_,globalB) => isTreeLike(globalB)
      case local => throw new RuntimeException(s"unexpected local type found in [$local]\n")
  end isTreeLike

  def apply(global:Global):Boolean =
    isTreeLike(global)
  end apply
end TreeLike