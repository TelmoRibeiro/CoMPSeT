package mpst.syntax

import mpst.syntax.Type.{Participant, Label, Sort, Variable}

/* @ telmo
  IDEA:
    => Protocol represents a session's natural structure
  ISSUES:
    => should Interaction(_,_,_,_) be considered an action as well?
    => toString is hiding sort
    => agentA shall turn to sender, agentB to receiver, message to label
  REVIEWED:
    => AFFIRMATIVE* => check Receive agent order
*/

/** Protocol represents a session's natural structure */
enum Protocol:
  override def toString: String =
    this match
      case Interaction(sender, receiver, label, _) => s"$sender>$receiver:$label"
      case Send   (sender, receiver, label, _) => s"$sender$receiver!$label"
      case Receive(receiver, sender, label, _) => s"$receiver$sender?$label"
      case RecursionCall(variable) => s"$variable"
      case Skip => s"skip"
      case Sequence(protocolA, protocolB) => s"$protocolA ; $protocolB"
      case Parallel(protocolA, protocolB) => s"($protocolA || $protocolB)"
      case Choice  (protocolA, protocolB) => s"($protocolA + $protocolB)"
      case RecursionFixedPoint(variable, protocolB) => s"def $variable in ($protocolB)"
  end toString

  case Interaction(sender: Participant, receiver: Participant, label: Label, sort: Sort)
  case Send   (sender: Participant, receiver: Participant, label: Label, sort: Sort)
  case Receive(receiver: Participant, sender: Participant, label: Label, sort: Sort)
  case RecursionCall(variable: Variable)
  case Skip
  case Sequence(protocolA: Protocol, protocolB: Protocol)
  case Parallel(protocolA: Protocol, protocolB: Protocol)
  case Choice  (protocolA: Protocol, protocolB: Protocol)
  case RecursionFixedPoint(variable: Variable, protocolB: Protocol)
end Protocol

/** Protocol represents a session's natural structure */
object Protocol:
  /**
   * Determines if a Protocol corresponds to a Global specification
   *  - Premise: the whole protocol should be passed as argument, otherwise, not enough information could be present to determine if it is a Global specification
   * */
  def isGlobal(protocol: Protocol): Boolean =
    protocol match
      case Interaction(_, _, _, _) => true
      case Send   (_, _, _, _) => false
      case Receive(_, _, _, _) => false
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case Parallel(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case Choice  (protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
      case RecursionFixedPoint(_, protocolB) => isGlobal(protocolB)
  end isGlobal

  /**
   * Determines if a Protocol corresponds to a Local specification
   *  - Premise: the whole protocol should be passed as argument, otherwise, not enough information could be present to determine if it is a Local specification
   * */
  def isLocal(protocol: Protocol): Boolean =
    protocol match
      case Interaction(_, _, _, _) => false
      case Send   (_, _, _, _) => true
      case Receive(_, _, _, _) => true
      case RecursionCall(_) => true
      case Skip => true
      case Sequence(protocolA, protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case Parallel(protocolA, protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case Choice  (protocolA, protocolB) => isLocal(protocolA) && isLocal(protocolB)
      case RecursionFixedPoint(_, protocolB) => isLocal(protocolB)
  end isLocal

  /** Collects all participants in Protocol */
  def getParticipants(protocol: Protocol): Set[Participant] =
    protocol match
      case Interaction(sender, receiver, _, _) => Set(sender, receiver)
      case Send(sender, receiver, _, _) => Set(sender, receiver)
      case Receive(receiver, sender, _, _) => Set(sender, receiver)
      case RecursionCall(_) => Set()
      case Skip => Set()
      case Sequence(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
      case Parallel(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
      case Choice(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
      case RecursionFixedPoint(_, protocolB) => getParticipants(protocolB)
  end getParticipants

  /** Determines if a Protocol corresponds to a Local action */
  def isAction(protocol: Protocol): Boolean =
    protocol match
      case Send   (_, _, _, _) => true
      case Receive(_, _, _, _) => true
      case _ => false
  end isAction

  /** Determines if a Protocol has the interleaving construct */
  def hasInterleaving(protocol: Protocol): Boolean =
    protocol match
      case Interaction(_, _, _, _) => false
      case Send   (_, _, _, _) => false
      case Receive(_, _, _, _) => false
      case RecursionCall(_) => false
      case Skip => false
      case Sequence(protocolA, protocolB) => hasInterleaving(protocolA) || hasInterleaving(protocolB)
      case Parallel(protocolA, protocolB) => true
      case Choice  (protocolA, protocolB) => hasInterleaving(protocolA) || hasInterleaving(protocolB)
      case RecursionFixedPoint(_, protocolB) => hasInterleaving(protocolB)
  end hasInterleaving
end Protocol