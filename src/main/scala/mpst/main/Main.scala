/*
package mpst.main

import mpst.operational_semantic.Network.*
import mpst.operational_semantic.SyncTraverse
import mpst.projection.*
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.utilities.Multiset
import mpst.wellformedness.*

import scala.annotation.tailrec
import scala.io.StdIn

/* IDEA:
  - console based tests
*/

object Main:
  @tailrec
  private def networkMSTraverse(locals:Set[(Agent,Local)],pending:Multiset[Action],trace:List[Action])(using environment:Map[Agent,Map[Variable,Protocol]]):Unit =
    val nextNetwork = NetworkMultiset.nextNetwork(locals,pending)
    for local <- locals yield
      println(s"LOCAL[${local._1}]: ${local._2}")
    println()
    for (nextAction,_,_) <- nextNetwork yield
      println(s"ACTION: $nextAction\n")
    if nextNetwork.toSeq.isEmpty then
      println(s"FINAL TRACE: $trace") ; return
    println(s"action index:")
    val actionIndex = StdIn.readInt( )
    val (a,ls,p) = nextNetwork.toSeq(actionIndex)
    println(s"TRACE: ${trace :+ a}")
    networkMSTraverse(ls,p,trace :+ a)
  end networkMSTraverse

  @tailrec
  private def networkCSTraverse(locals:Set[(Agent,Local)],pending:Queue,trace:List[Action])(using environment:Map[Agent,Map[Variable,Protocol]]):Unit =
    val nextNetwork = NetworkCausal.nextNetwork(locals,pending)
    for local <- locals yield
      println(s"LOCAL[${local._1}]: ${local._2}")
    println()
    for (nextAction,_,_) <- nextNetwork yield
      println(s"ACTION: $nextAction\n")
    if nextNetwork.toSeq.isEmpty then
      println(s"FINAL TRACE: $trace") ; return
    println(s"action index:")
    val actionIndex = StdIn.readInt( )
    val (a,ls,p) = nextNetwork.toSeq(actionIndex)
    println(s"TRACE: ${trace :+ a}")
    networkCSTraverse(ls,p,trace :+ a)
  end networkCSTraverse

  @tailrec
  private def syncTraverse(locals:Set[(Agent,Local)],trace:List[Action])(using environment:Map[Agent,Map[Variable,Local]]):Unit =
    val nextAction -> nextLocals = SyncTraverse.next(locals)
    println(s"ACTION: $nextAction")
    for local <- nextLocals yield
      println(s"LOCAL [${local._1}]: ${local._2}")
    println()
    println(s"TRACE: ${trace :+ nextAction}")
    var done = true
    for local <- nextLocals yield
    if local._2 != Skip then done = false
    if done then return
    syncTraverse(nextLocals,trace :+ nextAction)
  end syncTraverse

  def main(args: Array[String]): Unit =
    // val protocol = "(wA>m:Done<void> + m>wA:Done<void>)"
      // not accepted by me but accepted by oven
    // val protocol = "(m>wA:Done<void> + m>wB:Done<void>)"
      // not accepted by me or oven but accepted by choreo
    // val protocol = "(m>wA:Done<void> + (m>wA:NotDone<void> ; (m>wB:Done<void> + m>wB:NotDone<void>)))"
      // accepted by me but not oven
    // val protocol = "s>b:Descr<void> ; s>b:Price<void> ; (s>b:Acc<void> + s>b:Rej<void>) ; end"
      // should and it is accepted
    val protocol = "m>wA:Work<void> ; m>wB:Work<void> ; (wA>m:Done<void> ; end || wB>m:Done<void> ; end)"
      // should and it is accepted
    // val protocol = "m>wA:Work<void> ; m>wB:Work<void> ; wA>m:Done<void> ; wB>m:Done<void> ; end"
      // SYNC: should and it is accepted
    println(s"PROTOCOL: $protocol")
    val global = Parser(protocol)
    println(s"GLOBAL: $global")
    val wellformed =  WellCommunicated(global) && WellBounded(global) && WellChannelled(global) && WellBranched(global) && DependentlyGuarded(global)
    if !wellformed then throw new RuntimeException(s"not well formed!\n")
    val locals = for local <- AsyncProjection.projectionWithAgent(global) yield
      println(s"LOCAL [${local._1}] - ${local._2}")
      local
    val localsEnv = Environment.localEnv(global)
    // syncTraverse(locals,Nil)(using localsEnv)
    networkMSTraverse(locals,Multiset(),Nil)(using localsEnv)
  end main
end Main
*/