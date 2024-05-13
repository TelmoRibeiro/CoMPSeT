package mpst.syntax

import mpst.syntax.Type.{Agent,Message,Sort,Variable}

/* IDEA:
  - protocol basics...

  problem: should Interaction(_,_,_,_) be considered an action as well?
*/

enum Protocol:
  override def toString:String =
    this match
      case Interaction(agentA,agentB,message,sort) => s"$agentA>$agentB:$message<$sort>"
      case Send   (agentA,agentB,message,sort) => s"$agentA$agentB!$message<$sort>"
      case Receive(agentA,agentB,message,sort) => s"$agentA$agentB?$message<$sort>"
      case RecursionCall(variable) => s"$variable"
      case Skip => s"skip"
      case Sequence(protocolA,protocolB) => s"$protocolA ; $protocolB"
      case Parallel(protocolA,protocolB) => s"($protocolA || $protocolB)"
      case Choice  (protocolA,protocolB) => s"($protocolA + $protocolB)"
      case RecursionFixedPoint(variable,protocolB) => s"def $variable in ($protocolB)"
  end toString

  case Interaction(agentA:Agent,agentB:Agent,message:Message,sort:Sort)
  case Send       (agentA:Agent,agentB:Agent,message:Message,sort:Sort)
  case Receive    (agentA:Agent,agentB:Agent,message:Message,sort:Sort)
  case RecursionCall(variable:Variable)
  case Skip
  case Sequence(protocolA:Protocol,protocolB:Protocol)
  case Parallel(protocolA:Protocol,protocolB:Protocol)
  case Choice  (protocolA:Protocol,protocolB:Protocol)
  case RecursionFixedPoint(variable:Variable,protocolB:Protocol)
end Protocol

object Protocol:
  def isGlobal(protocol:Protocol):Boolean =
    protocol match
      case Interaction(_,_,_,_) => true
      case Send   (_,_,_,_) => false
      case Receive(_,_,_,_) => false
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(protocolA,protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case Parallel(protocolA,protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case Choice  (protocolA,protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case RecursionFixedPoint(_,protocolB) => isGlobal(protocolB)
  end isGlobal

  def isLocal(protocol:Protocol):Boolean =
    protocol match
      case Interaction(_,_,_,_) => false
      case Send   (_,_,_,_) => true
      case Receive(_,_,_,_) => true
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(protocolA,protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case Parallel(protocolA,protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case Choice  (protocolA,protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case RecursionFixedPoint(_,protocolB) => isLocal(protocolB)
  end isLocal

  def isAction(protocol:Protocol):Boolean =
    protocol match
      case Send   (_,_,_,_) => true
      case Receive(_,_,_,_) => true
      case _ => false
  end isAction

  def getAgents(protocol:Protocol):Set[Agent] =
    protocol match
      case Interaction(agentA,agentB,_,_) => (Set() + agentA) + agentB
      case Send   (agentA,agentB,_,_) => (Set() + agentA) + agentB
      case Receive(agentA,agentB,_,_) => (Set() + agentA) + agentB
      case RecursionCall(_) => Set()
      case Skip => Set()
      case Sequence(protocolA,protocolB) => getAgents(protocolA) ++ getAgents(protocolB)
      case Parallel(protocolA,protocolB) => getAgents(protocolA) ++ getAgents(protocolB)
      case Choice  (protocolA,protocolB) => getAgents(protocolA) ++ getAgents(protocolB)
      case RecursionFixedPoint(_,protocolB) => getAgents(protocolB)
  end getAgents
end Protocol