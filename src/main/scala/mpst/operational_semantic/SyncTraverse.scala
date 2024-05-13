package mpst.operational_semantic

import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

/* IDEA:
  - provide a way to neatly traverse SYNC semantics
  - *similar* (but no quite) to Network in that regard
  - check with prof. José Proença

  problem: locals are being treated naively
*/

object SyncTraverse:
  def next(locals:Set[(Agent,Local)])(using localEnv:Map[Agent,Map[Variable,Local]]):(Action,Set[(Agent,Local)]) =
    val (sendNextAction,sendLocal,sendNextLocal) = getSend(locals)
    val (recvNextAction,recvLocal,recvNextLocal) = getRecv(locals,sendNextAction)
    val nextLocals = locals - sendLocal - recvLocal + sendNextLocal + recvNextLocal
    sendNextAction -> nextLocals
  end next

  private def getSend(locals:Set[(Agent,Local)])(using localEnv:Map[Agent,Map[Variable,Local]]):(Action,(Agent,Local),(Agent,Local)) =
    def canSend(action:Action,locals:Set[(Agent,Local)]):Boolean =
      if !isAction(action) then throw new RuntimeException(s"unexpected protocol found in [$action]\n")
      action match
        // @ telmo - that means we are in a situation of the type: a!b:m<s>;G1 | b?a:m<s>;G2
        case Send(agentA,agentB,message,sort) =>
          val receives = for local <- locals yield
            for recv -> _ <- MPSTSemantic.next(local._2)(using localEnv(local._1)) if recv == Receive(agentB,agentA,message,sort) yield
              recv
          val flatReceives = receives.flatten
          if flatReceives.size > 1 then throw new RuntimeException(s"unexpected ambiguous receives in [$flatReceives]\n") // @ telmo - maybe I do not need to worry with this here
          flatReceives.nonEmpty
        case _ => false
    end canSend
    val next = for local <- locals yield
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using localEnv(local._1)) if canSend(nextAction,locals) yield
        (nextAction,local,local._1 -> nextLocal)
    val flattenNext = next.flatten
    flattenNext.head
  end getSend

  private def getRecv(locals:Set[(Agent,Local)],send:Action)(using localEnv:Map[Agent,Map[Variable,Local]]):(Action,(Agent,Local),(Agent,Local)) =
    def canRecv(action:Action,send:Action):Boolean =
      if !isAction(action) then throw new RuntimeException(s"unexpected protocol found in [$action]\n")
        action match
        // @ telmo - that means we are in a situation of the type: a!b:m<s>;G1 | b?a:m<s>;G2
        case Receive(agentA,agentB,message,sort) if send == Send(agentB,agentA,message,sort) => true
        case _  => false
    end canRecv
    val next = for local <- locals yield
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using localEnv(local._1)) if canRecv(nextAction,send) yield
        (nextAction,local,local._1 -> nextLocal)
    val flattenNext = next.flatten
    if flattenNext.size != 1 then throw new RuntimeException(s"unexpected ambiguous actions found in [$flattenNext]\n") // @ telmo - maybe I do not need to worry with this here
    flattenNext.head
  end getRecv
end SyncTraverse