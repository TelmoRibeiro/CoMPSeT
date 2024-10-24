package mpst.syntax

/* @ telmo
  IDEA:
    => [[Protocol]] represents a session's natural structure ([[AST]] of constructs).
  ISSUES:
    => current implementations allows for Sort but do not use it.
        As such, it is mostly ignored/hidden as with the case of toString.
    => should Interaction(_,_,_,_) be considered an Action as well?
  REVIEWED:
    => AFFIRMATIVE
*/

enum Protocol:
  override def toString: String = this match
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

  // constructs allowed by our syntax //
  case Interaction(sender: Protocol.Participant, receiver: Protocol.Participant, label: Protocol.Label, sort: Protocol.Sort)
  case Send   (sender: Protocol.Participant, receiver: Protocol.Participant, label: Protocol.Label, sort: Protocol.Sort)
  case Receive(receiver: Protocol.Participant, sender: Protocol.Participant, label: Protocol.Label, sort: Protocol.Sort)
  case RecursionCall(variable: Protocol.Variable)
  case Skip
  case Sequence(protocolA: Protocol, protocolB: Protocol)
  case Parallel(protocolA: Protocol, protocolB: Protocol)
  case Choice  (protocolA: Protocol, protocolB: Protocol)
  case RecursionFixedPoint(variable: Protocol.Variable, protocolB: Protocol)
end Protocol

object Protocol:
  // global & local Types //
  type Global = Protocol
  type Local  = Protocol

  // internal types //
  type Participant = String
  type Label       = String
  type Sort        = String
  type Variable    = String

  // semantic types //
  type Action = Protocol

  def isGlobal(protocol: Protocol): Boolean = protocol match
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

  def isLocal(protocol: Protocol): Boolean = protocol match
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

  def getParticipants(protocol: Protocol): Set[Participant] = protocol match
    case Interaction(sender, receiver, _, _) => Set(sender, receiver)
    case Send   (sender, receiver, _, _) => Set(sender, receiver)
    case Receive(receiver, sender, _, _) => Set(sender, receiver)
    case RecursionCall(_) => Set.empty
    case Skip => Set.empty
    case Sequence(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case Parallel(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case Choice  (protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case RecursionFixedPoint(_, protocolB) => getParticipants(protocolB)
  end getParticipants

  def isAction(protocol: Protocol): Boolean = protocol match
    case Send   (_, _, _, _) => true
    case Receive(_, _, _, _) => true
    case _ => false
  end isAction

  def hasInterleaving(protocol: Protocol): Boolean = protocol match
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