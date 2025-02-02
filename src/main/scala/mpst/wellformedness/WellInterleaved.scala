package mpst.wellformedness

import mpst.syntax.Protocol.*

object WellInterleaved:
  private def isWellInterleaved(global: Global): Boolean = global match
    case _: Interaction | _: RecursionCall | Skip => true
    case Sequence(globalA, globalB) => isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case Parallel(globalA, globalB) => isWellInterleavedAuxiliary(globalA, globalB) && isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case Choice  (globalA, globalB) => isWellInterleaved(globalA) && isWellInterleaved(globalB)
    case RecursionFixedPoint(_, globalB) => isWellInterleaved(globalB)
    case RecursionKleeneStar(globalA)    => isWellInterleaved(globalA)
    case _ => throw RuntimeException(s"found unexpected {$global}")
  end isWellInterleaved

  private def isWellInterleavedAuxiliary(globalA: Global, globalB: Global): Boolean = hasRecursion(globalA) -> hasRecursion(globalB) match
    case true -> _ => throw RuntimeException(s"found recursion in [$globalA]")
    case _ -> true => throw RuntimeException(s"found recursion in [$globalB]")
    case _ => true
  end isWellInterleavedAuxiliary

  def apply(global: Global): Boolean =
    isWellInterleaved(global)
  end apply
end WellInterleaved