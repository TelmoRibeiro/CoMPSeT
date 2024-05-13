package mpst.wellformedness

import mpst.operational_semantic.MPSTSemantic
import mpst.projection.SyncProjection.*       // @ telmo - testing for Sync
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.Environment.*

/* IDEA:
  - check well-formedness on *choice*
  - @ telmo - expand this
*/

object WellBranched:
  private def isWellBranched(global:Global)(using environment:Map[String,Global]):Boolean =
    global match
      case Interaction(_,_,_,_) => true
      case RecursionCall(_)     => true
      case Skip                 => true
      case Sequence(globalA,globalB) => isWellBranched(globalA) && isWellBranched(globalB)
      case Parallel(globalA,globalB) => isWellBranched(globalA) && isWellBranched(globalB)
      case Choice  (globalA,globalB) => checkWellBranched(globalA,globalB) && isWellBranched(globalA) && isWellBranched(globalB)
      case RecursionFixedPoint(_,globalB) => isWellBranched(globalB)
      case local => throw new RuntimeException(s"unexpected local type found in [$local]\n")
  end isWellBranched

  private def checkWellBranched(globalA:Global,globalB:Global)(using environment:Map[Variable,Global]):Boolean =
    // @ telmo - nextActionsX = actions produced in reducing globalX
    val nextActionsA = for actionsA -> stateA <- MPSTSemantic.next(globalA) yield actionsA
    val nextActionsB = for actionsB -> stateB <- MPSTSemantic.next(globalB) yield actionsB
    val selectors = for case Send(agentA,_,_,_) <- nextActionsA ++ nextActionsB yield agentA
    if selectors.isEmpty  then throw new RuntimeException(s"no selector in [$globalA] and [$globalB]\n")
    if selectors.size > 1 then throw new RuntimeException(s"multiple selectors in [$globalA] and [$globalB]\n")
    val selector = selectors.head
    // @ telmo - may need to pay attention to the sort
    // @ telmo - sendingX = receiver and messages from actions being send by selector in actionX
    val sendingActionsA = for case Send(selector,agentB,message,_) <- nextActionsA yield agentB -> message
    val sendingActionsB = for case Send(selector,agentB,message,_) <- nextActionsB yield agentB -> message
    // @ telmo - receivingX = receiver and message from action being received by selector in actionX
    val receivingActionsA = receivingActions(globalA,sendingActionsA)
    val receivingActionsB = receivingActions(globalB,sendingActionsB)
    // @ telmo - all global sends have a local leading receive
    for sendingActionA <- sendingActionsA if !receivingActionsA.contains(sendingActionA) yield
      throw new RuntimeException(s"$selector>${sendingActionA._1}${sendingActionA._2} in [$globalA] cannot be received\n")
    for sendingActionB <- sendingActionsB if !receivingActionsB.contains(sendingActionB) yield
      throw new RuntimeException(s"$selector>${sendingActionB._1}${sendingActionB._2} in [$globalB] cannot be received\n")
    // @ telmo - receiving agents must be the same in both branches but the messages must be different
    // @ telmo - each receiving "agentB:mA" @ globalA have a matching "agentB:mB" @ globalB where mA != mB
    // @ telmo - fails because mA == mB
    for agentB -> messageToAgentB <- receivingActionsA yield
      if receivingActionsB.contains(agentB -> messageToAgentB)
      then throw new RuntimeException(s"$selector>$agentB$messageToAgentB is ambiguous in [$globalA] and [$globalB]\n")
    // @ telmo - fails because there is no "agentB:mB" @ globalB
    for agentB -> messageToAgentB <- receivingActionsA yield
      if !receivingActionsB.map(_._1).contains(agentB)
      then throw new RuntimeException(s"$selector>$agentB$messageToAgentB from [$globalA] cannot be matched in [$globalB]\n")
    // @ telmo - fails because there is no "agentB:mA" @ globalA
    for agentB -> messageToAgentB <- receivingActionsB yield
      if !receivingActionsA.map(_._1).contains(agentB)
      then throw new RuntimeException(s"$selector>$agentB$messageToAgentB from [$globalB] cannot be matched in [$globalA]\n")
    true
  end checkWellBranched

  private def receivingActions(global:Global,sendingActions:Set[(Agent,Message)])(using environment:Map[Variable,Global]):Set[(Agent,Message)] =
    for agent -> local <- projectionWithAgent(global)
        case Receive(agent,selector,message,_) -> _ <- MPSTSemantic.next(local) if sendingActions contains agent -> message
    yield agent -> message
  end receivingActions

  def apply(global:Global):Boolean =
    val environment = globalEnv(global)
    isWellBranched(global)(using environment)
end WellBranched

/*
  @ telmo -
  previous implementation

object Disambiguation:
  @tailrec
  private def roleDisambiguation(globalA: Protocol, globalB: Protocol, roles: Set[String], isStillProjectable: Boolean = true): Boolean =
    if   roles.isEmpty
    then isStillProjectable
    else
      val role: String               = roles.head
      val headGlobalA: Set[Protocol] = headInteraction(globalA)(using role)
      val headGlobalB: Set[Protocol] = headInteraction(globalB)(using role)
      val isProjectable: Boolean     = (headGlobalA intersect headGlobalB).isEmpty
      roleDisambiguation(globalA, globalB, roles - role, isStillProjectable && isProjectable)
  end roleDisambiguation

  private def disambiguation(global: Protocol): Boolean =
    global match
      // terminal cases //
      case Interaction  (_, _, _, _) => true
      case RecursionCall(_) => true
      case End              => true
      // recursive cases //
      case RecursionFixedPoint(_, globalB) => disambiguation(globalB)
      case Sequence(globalA, globalB)      => disambiguation(globalA) && disambiguation(globalB)
      case Parallel(globalA, globalB)      => disambiguation(globalA) && disambiguation(globalB)
      case   Choice(globalA, globalB)      => roleDisambiguation(globalA, globalB, getAgents(global))
      // unexpected cases //
      case local => throw new RuntimeException(s"unexpected local type $local found\n")
  end disambiguation

  def apply(global: Protocol): Boolean = disambiguation(global)
end Disambiguation
*/