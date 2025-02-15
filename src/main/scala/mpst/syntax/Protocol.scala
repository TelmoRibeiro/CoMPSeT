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
    case RecursionKleeneStar(protocolA) => s"($protocolA)*"
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
  case RecursionKleeneStar(protocolA: Protocol)
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
  type Action = Send | Receive

  def isGlobal(protocol: Global): Boolean = protocol match
    case _: Interaction | _: RecursionCall | Skip => true
    case _: Send | _: Receive => false
    case Sequence(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case Parallel(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case Choice  (protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case RecursionFixedPoint(_, protocolB) => isGlobal(protocolB)
    case RecursionKleeneStar(protocolA)    => isGlobal(protocolA)
  end isGlobal

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
    case RecursionKleeneStar(protocolA)    => getParticipants(protocolA)
  end getParticipants

  def hasParallel(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Send | _: Receive | _: RecursionCall | Skip => false
    case _: Parallel => true
    case Sequence(protocolA, protocolB) => hasParallel(protocolA) || hasParallel(protocolB)
    case Choice  (protocolA, protocolB) => hasParallel(protocolA) || hasParallel(protocolB)
    case RecursionFixedPoint(_, protocolB) => hasParallel(protocolB)
    case RecursionKleeneStar(protocolA)    => hasParallel(protocolA)
  end hasParallel

  def hasKleeneStarRecursion(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Send | _: Receive | _: RecursionCall | Skip => false
    case _: RecursionKleeneStar => true
    case Sequence(protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case Parallel(protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case Choice  (protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case RecursionFixedPoint(_, protocolB) => hasKleeneStarRecursion(protocolB)
  end hasKleeneStarRecursion

  def hasFixedPointRecursion(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Send | _: Receive | _: RecursionCall | Skip => false // @ telmo - should de presence of RecursionCall render it true as well
    case _: RecursionFixedPoint => true
    case Sequence(protocolA, protocolB) => hasFixedPointRecursion(protocolA) || hasFixedPointRecursion(protocolB)
    case Parallel(protocolA, protocolB) => hasFixedPointRecursion(protocolA) || hasFixedPointRecursion(protocolB)
    case Choice  (protocolA, protocolB) => hasFixedPointRecursion(protocolA) || hasFixedPointRecursion(protocolB)
    case RecursionKleeneStar(protocolA) => hasFixedPointRecursion(protocolA)
  end hasFixedPointRecursion
  
  def hasRecursion(protocol: Protocol): Boolean =
    hasKleeneStarRecursion(protocol) || hasFixedPointRecursion(protocol)
  end hasRecursion
end Protocol