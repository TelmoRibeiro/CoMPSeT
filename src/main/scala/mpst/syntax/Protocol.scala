package mpst.syntax


enum Protocol:
  override def toString: String = this match
    case Interaction(sender, receiver, label) => s"$sender->$receiver:$label"
    case Send(_, receiver, label) => s"$receiver!$label"
    case Recv(_, sender, label) => s"$sender?$label"
    case RecursionCall(variable) => s"$variable"
    case Skip => s"skip"
    case Sequence(protocolA, protocolB) => s"$protocolA ; $protocolB"
    case Parallel(protocolA, protocolB) => s"($protocolA || $protocolB)"
    case Choice  (protocolA, protocolB) => s"($protocolA + $protocolB)"
    case RecursionFixedPoint(variable, protocolB) => s"def $variable in $protocolB"
    case RecursionKleeneStar(protocolA) => s"($protocolA)*"
  end toString

  // constructs allowed by our syntax //
  case Interaction(sender: Protocol.Participant, receiver: Protocol.Participant, label: Protocol.Label)
  case Send(sender: Protocol.Participant, receiver: Protocol.Participant, label: Protocol.Label)
  case Recv(receiver: Protocol.Participant, sender: Protocol.Participant, label: Protocol.Label)
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
  type Variable    = String

  // semantic types //
  type Action = Send | Recv

  def isGlobal(protocol: Global): Boolean = protocol match
    case _: Interaction | _: RecursionCall | Skip => true
    case _: Send | _: Recv => false
    case Sequence(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case Parallel(protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case Choice  (protocolA, protocolB) => isGlobal(protocolA) && isGlobal(protocolB)
    case RecursionFixedPoint(_, protocolB) => isGlobal(protocolB)
    case RecursionKleeneStar(protocolA)    => isGlobal(protocolA)
  end isGlobal

  def matchingAction(action: Action): Action = action match
    case sendAction: Send => Recv(sendAction.receiver, sendAction.sender, sendAction.label)
    case recvAction: Recv => Send(recvAction.sender, recvAction.receiver, recvAction.label)
  end matchingAction

  def getParticipants(protocol: Protocol): Set[Participant] = protocol match
    case interaction: Interaction => Set(interaction.sender, interaction.receiver)
    case sendAction: Send => Set(sendAction.sender,  sendAction.receiver)
    case recvAction: Recv => Set(recvAction.sender,  recvAction.receiver)
    case _: RecursionCall | Skip => Set.empty
    case Sequence(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case Parallel(protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case Choice  (protocolA, protocolB) => getParticipants(protocolA) ++ getParticipants(protocolB)
    case RecursionFixedPoint(_, protocolB) => getParticipants(protocolB)
    case RecursionKleeneStar(protocolA)    => getParticipants(protocolA)
  end getParticipants

  def hasParallel(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Action | _: RecursionCall | Skip => false
    case _: Parallel => true
    case Sequence(protocolA, protocolB) => hasParallel(protocolA) || hasParallel(protocolB)
    case Choice  (protocolA, protocolB) => hasParallel(protocolA) || hasParallel(protocolB)
    case RecursionFixedPoint(_, protocolB) => hasParallel(protocolB)
    case RecursionKleeneStar(protocolA)    => hasParallel(protocolA)
  end hasParallel

  def hasKleeneStarRecursion(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Action | _: RecursionCall | Skip => false
    case _: RecursionKleeneStar => true
    case Sequence(protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case Parallel(protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case Choice  (protocolA, protocolB) => hasKleeneStarRecursion(protocolA) || hasKleeneStarRecursion(protocolB)
    case RecursionFixedPoint(_, protocolB) => hasKleeneStarRecursion(protocolB)
  end hasKleeneStarRecursion

  def hasFixedPointRecursion(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Action | _: RecursionCall | Skip => false
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