package mpst.encoding

import mpst.operational_semantic.MPSTSemantic
import mpst.syntax.Protocol
import mpst.syntax.Type.*

/* IDEA:
  - no encoding
  - just an hacky/lazy way to traverse MPSTSemantic before Network was implemented
  - use was never recommended
*/

@deprecated
object NoEncoding:
  private def lazyTraverse(next:Set[(Action,Protocol)],visited:Set[(Action,Protocol)])(using environment:Map[Variable,Protocol]):Unit =
    if next.isEmpty then
      println("done traversing")
      return
    val nextAction -> nextLocal = next.head
    println(s"Action: $nextAction")
    println(s" Local: $nextLocal")
    if visited contains next.head then
      println("reduction was visited already\n")
      lazyTraverse(next.tail,visited)
      return
    println()
    val children = MPSTSemantic.next(nextLocal)
    lazyTraverse(children ++ next.tail,visited + next.head)
  end lazyTraverse

  /* @ telmo - add new env
  def apply(local:Protocol):Unit =
    val localEnv = Environment.localEnv(local)
    lazyTraverse(MPSTSemantic.next(local)(using localEnv),Set())(using environment)
  end apply
  */
end NoEncoding