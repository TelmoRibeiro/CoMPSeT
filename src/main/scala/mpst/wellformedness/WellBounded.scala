package mpst.wellformedness

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

/*
  IDEA:
    - checks well-formedness on *recursion*
      - is every recursion variable bounded?
      - different branches of parallel can't use the same variable
        - from LessIsMore

  problem:
    are we bothered by unused variables?

    prof. Jose Proença checks WellBounded
      through communication graphs
      checks if the graph is strongly connected
      the object of study was global* not def X in global
      in "def X in global" is it not as simple as being sure every X is a bounded variable?
*/

object WellBounded:
  private def getFreeVariables(global:Global)(using declaredVariables:Set[Variable]):Set[Variable] =
    global match
      case Interaction(_,_,_,_) => Set()
      case RecursionCall(variable) =>
        if declaredVariables contains variable
        then Set()
        else Set() + variable
      case Skip => Set()
      case Sequence(globalA,globalB) => getFreeVariables(globalA) ++ getFreeVariables(globalB)
      case Parallel(globalA,globalB) => getFreeVariables(globalA)(using Set()) ++ getFreeVariables(globalB)(using Set())
      case Choice  (globalA,globalB) => getFreeVariables(globalA) ++ getFreeVariables(globalB)
      case RecursionFixedPoint(variable,globalB) => getFreeVariables(globalB)(using declaredVariables ++ Set(variable))
      case local => throw new RuntimeException(s"unexpected local type found in [$local]\n")
  end getFreeVariables

  def apply(global:Global):Boolean =
    val freeVariables = getFreeVariables(global)(using Set())
    if  freeVariables.nonEmpty then throw new RuntimeException(s"unexpected free variables [$freeVariables] in [$global]\n")
    true
  end apply
end WellBounded

/*
  @ telmo -
  previous implementation

object FreeVariables:
  private def freeVariables(variables: Set[String], global: Protocol): Boolean =
    global match
      // terminal cases //
      case Interaction  (_, _, _, _)  => true
      case RecursionCall(variable) => variables.contains(variable)
      case End                     => true
      // recursive cases //
      case RecursionFixedPoint(variable, globalB) => freeVariables(variables + variable, globalB)
      case Sequence(globalA, globalB)  =>
        globalA match
          case RecursionCall(variable) => freeVariables(variables, globalA) && freeVariables(variables - variable, globalB)
          case _                       => freeVariables(variables, globalA) && freeVariables(variables, globalB)
      case Parallel(globalA, globalB)  => freeVariables(Set(), globalA)     && freeVariables(Set(), globalB)
      case Choice  (globalA, globalB)  => freeVariables(variables, globalA) && freeVariables(variables, globalB)
      // unexpected cases //
      case local => throw new RuntimeException(s"unexpected local type $local found\n")
  end freeVariables

  def apply(global: Protocol): Boolean = freeVariables(Set(), global)
end FreeVariables
*/