package mpst.utilities

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

/* IDEA:
  - attempt at renaming the whole recursion before working with it

  @ telmo -
    not completed/tested
    problematic...
*/

@deprecated
object RenameRecursion:
  // @ telmo - need to check if variableRenamed already does not exist before renameCall
  // @ telmo - "using" not helping since both "using" have the same type
  private def renameCall(local:Protocol)(using toRenameVariable:String)(using renamedVariable:String):Protocol =
    local match
      case Send   (_,_,_,_) => local
      case Receive(_,_,_,_) => local
      case RecursionCall(variable) =>
        if variable == toRenameVariable
        then RecursionCall(renamedVariable)
        else local
      case Skip => local
      case Sequence(localA,localB) =>
        val renamedLocalA = renameCall(localA)(using toRenameVariable)(using renamedVariable)
        val renamedLocalB = renameCall(localB)(using toRenameVariable)(using renamedVariable)
        Sequence(renamedLocalA,renamedLocalB)
      case Parallel(localA,localB) =>
        val renamedLocalA = renameCall(localA)(using toRenameVariable)(using renamedVariable)
        val renamedLocalB = renameCall(localB)(using toRenameVariable)(using renamedVariable)
        Parallel(renamedLocalA,renamedLocalB)
      case Choice(localA,localB) =>
        val renamedLocalA = renameCall(localA)(using toRenameVariable)(using renamedVariable)
        val renamedLocalB = renameCall(localB)(using toRenameVariable)(using renamedVariable)
        Choice(renamedLocalA,renamedLocalB)
      case RecursionFixedPoint(variable,localB) =>
        if variable == toRenameVariable
        then throw new RuntimeException(s"unexpected variable name found in $local")
        else
          val renamedLocalB = renameCall(localB)(using toRenameVariable)(using renamedVariable)
          RecursionFixedPoint(variable,renamedLocalB)
      case global => throw new RuntimeException(s"unexpected global type found in $global")
  end renameCall

  private def renameRecursion(local:Protocol)(using counter:Int):(Protocol,Int) =
    local match
      case Send   (_,_,_,_) => local -> counter
      case Receive(_,_,_,_) => local -> counter
      case RecursionCall(_) => local -> counter
      case Skip => local -> counter
      case Sequence(localA,localB) =>
        val renamedLocalA -> counterA = renameRecursion(localA)
        val renamedLocalB -> counterB = renameRecursion(localB)(using counterA)
        Sequence(renamedLocalA,renamedLocalB) -> counterB
      case Parallel(localA,localB) =>
        val renamedLocalA -> counterA = renameRecursion(localA)
        val renamedLocalB -> counterB = renameRecursion(localB)(using counterA)
        Parallel(renamedLocalA,renamedLocalB) -> counterB
      case Choice  (localA,localB) =>
        val renamedLocalA -> counterA = renameRecursion(localA)
        val renamedLocalB -> counterB = renameRecursion(localB)(using counterA)
        Choice(renamedLocalA,renamedLocalB) -> counterB
      case RecursionFixedPoint(variable,localB) =>
        val renamedVariable = variable+"_"+counter
        val renamedLocalB   = renameCall(localB)(using variable)(using renamedVariable)
        RecursionFixedPoint(renamedVariable,renamedLocalB) -> (counter + 1)
      case global => throw new RuntimeException(s"unexpected global type found in $global")
  end renameRecursion

  def apply(local:Protocol):Protocol =
    val renamedLocal -> counter = renameRecursion(local)(using 0)
    renamedLocal
  end apply
end RenameRecursion