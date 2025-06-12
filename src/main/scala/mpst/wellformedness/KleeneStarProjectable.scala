package mpst.wellformedness

import mpst.syntax.Protocol.*


object KleeneStarProjectable:
  private def isKleeneStarProjectable(global: Global): Boolean = global match
    case _: Interaction | _: Action | Skip | _: RecursionCall => true
    case Sequence(globalA, globalB) => globalA match
      case _: RecursionKleeneStar => throw RuntimeException(s"KP-consistent check is not yet supported for [$global]")
      case _ => isKleeneStarProjectable(globalA) && isKleeneStarProjectable(globalB)
    case Parallel(globalA, globalB) => isKleeneStarProjectable(globalA) && isKleeneStarProjectable(globalB)
    case Choice  (globalA, globalB) => isKleeneStarProjectable(globalA) && isKleeneStarProjectable(globalB)
    case RecursionFixedPoint(_, globalB) => isKleeneStarProjectable(globalB)
    case RecursionKleeneStar(globalA)    => isKleeneStarProjectable(globalA)
  end isKleeneStarProjectable

  def apply(global: Global): Boolean =
    isKleeneStarProjectable(global)
  end apply
end KleeneStarProjectable