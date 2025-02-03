package mpst.wellformedness

import mpst.syntax.Protocol.*

object WellBounded:
  private def getBoundedVariables(global: Global)(using boundedVariables: Set[Variable]): Set[Variable] = global match
    case _: Interaction | Skip =>
      boundedVariables
    case RecursionCall(variable) =>
      if   boundedVariables.contains(variable)
      then boundedVariables
      else throw RuntimeException(s"found unbounded [$variable]")
    case Sequence(globalA, globalB) =>
      getBoundedVariables(globalB)(using getBoundedVariables(globalA))
    case Parallel(globalA, globalB) =>
      getBoundedVariables(globalA)(using Set.empty)
      getBoundedVariables(globalB)(using Set.empty)
      boundedVariables
    case Choice  (globalA, globalB) =>
      getBoundedVariables(globalA)
      getBoundedVariables(globalB)
      boundedVariables
    case RecursionFixedPoint(variable, globalB) =>
      getBoundedVariables(globalB)(using boundedVariables + variable)
    case RecursionKleeneStar(globalA) =>
      getBoundedVariables(globalA)
    case _ => throw RuntimeException(s"found unexpected [$global]")
  end getBoundedVariables

  def apply(global: Global): Boolean =
    getBoundedVariables(global)(using Set.empty)
    true
  end apply
end WellBounded