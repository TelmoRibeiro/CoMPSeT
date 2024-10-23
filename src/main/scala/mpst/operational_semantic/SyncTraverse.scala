package mpst.operational_semantic

import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.Environment.{localEnv, singleLocalEnv}

/* IDEA:
  - provide a way to neatly traverse SYNC semantics
  - *similar* (but no quite) to Network in that regard
  - check with prof. José Proença

  problem: locals are being treated naively
*/

object SyncTraverse:
  def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
    localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
  end accepting

  def next[A >: Action](locals: Set[(Participant, Local)])(using environment: Environment): (A, Set[(Participant, Local)]) =
    val (sendNextAction, sendLocal, sendNextLocal) = getSend(locals)
    val (recvNextAction, recvLocal, recvNextLocal) = getRecv(locals, sendNextAction)
    val nextLocals = locals - sendLocal - recvLocal + sendNextLocal + recvNextLocal
    sendNextAction -> nextLocals
  end next

  private def getSend(locals: Set[(Participant, Local)])(using environment: Environment): (Action, (Participant, Local), (Participant, Local)) =
    def canSend(action: Action, locals: Set[(Participant, Local)]): Boolean =
      if !isAction(action) then throw new RuntimeException(s"unexpected protocol found in [$action]\n")
      action match
        // @ telmo - that means we are in a situation of the type: a!b:m<s>;G1 | b?a:m<s>;G2
        case Send(agentA,agentB,message,sort) =>
          val receives = for local <- locals yield
            for recv -> _ <- MPSTSemantic.next(local._2)(using singleLocalEnv(local._2)) if recv == Receive(agentB,agentA,message,sort) yield
              recv
          val flatReceives = receives.flatten
          if flatReceives.size > 1 then throw new RuntimeException(s"unexpected ambiguous receives in [$flatReceives]\n") // @ telmo - maybe I do not need to worry with this here
          flatReceives.nonEmpty
        case _ => false
    end canSend
    val next = for local <- locals yield
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using singleLocalEnv(local._2)) if canSend(nextAction,locals) yield
        (nextAction,local,local._1 -> nextLocal)
    val flattenNext = next.flatten
    flattenNext.head
  end getSend

  private def getRecv(locals: Set[(Participant, Local)], send: Action)(using environment: Environment): (Action, (Participant, Local), (Participant, Local)) =
    def canRecv(action: Action, send: Action): Boolean =
      if !isAction(action) then throw new RuntimeException(s"unexpected protocol found in [$action]\n")
        action match
        // @ telmo - that means we are in a situation of the type: a!b:m<s>;G1 | b?a:m<s>;G2
        case Receive(agentA,agentB,message,sort) if send == Send(agentB,agentA,message,sort) => true
        case _  => false
    end canRecv
    val next = for local <- locals yield
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using environment(local._1)) if canRecv(nextAction,send) yield
        (nextAction,local,local._1 -> nextLocal)
    val flattenNext = next.flatten
    if flattenNext.size != 1 then throw new RuntimeException(s"unexpected ambiguous actions found in [$flattenNext]\n") // @ telmo - maybe I do not need to worry with this here
    flattenNext.head
  end getRecv
end SyncTraverse