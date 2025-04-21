package mpst.syntax


enum Protocol:
  def toSimpleString: String = this match
    case Interaction(sender, receiver, label) => s"$sender->$receiver:$label"
    case Send(_, receiver, label) => s"$receiver!$label"
    case Recv(_, sender, label)   => s"$sender?$label"
    case RecursionCall(variable) => s"$variable"
    case Skip => s"skip"
    case Sequence(protocolA, protocolB) => s"${protocolA.toSimpleString} ; ${protocolB.toSimpleString}"
    case Parallel(protocolA, protocolB) => s"(${protocolA.toSimpleString} || ${protocolB.toSimpleString})"
    case Choice  (protocolA, protocolB) => s"(${protocolA.toSimpleString} + ${protocolB.toSimpleString})"
    case RecursionFixedPoint(variable, protocolB) => s"def $variable in ${protocolB.toSimpleString}"
    case RecursionKleeneStar(protocolA) => s"(${protocolA.toSimpleString})*"
  end toSimpleString

  override def toString: String = this match
    case Interaction(sender, receiver, label) => s"$sender->$receiver:$label"
    case Send(sender, receiver, label) => s"$sender-$receiver!$label"
    case Recv(receiver, sender, label) => s"$receiver-$sender?$label"
    case RecursionCall(variable) => s"$variable"
    case Skip => s"skip"
    case Sequence(protocolA, protocolB) => s"$protocolA ; $protocolB"
    case Parallel(protocolA, protocolB) => s"($protocolA || $protocolB)"
    case Choice  (protocolA, protocolB) => s"($protocolA + $protocolB)"
    case RecursionFixedPoint(variable, protocolB) => s"def $variable in $protocolB"
    case RecursionKleeneStar(protocolA) => s"($protocolA)*"
  end toString

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
  type Global = Protocol
  type Local  = Protocol

  type Participant = String
  type Label       = String
  type Variable    = String

  type Action = Send | Recv

  def isGlobal(global: Global): Boolean = global match
    case _: Interaction | _: RecursionCall | Skip => true
    case _: Action => false
    case Sequence(globalA, globalB) => isGlobal(globalA) && isGlobal(globalB)
    case Parallel(globalA, globalB) => isGlobal(globalA) && isGlobal(globalB)
    case Choice  (globalA, globalB) => isGlobal(globalA) && isGlobal(globalB)
    case RecursionFixedPoint(_, globalB) => isGlobal(globalB)
    case RecursionKleeneStar(globalA)    => isGlobal(globalA)
  end isGlobal

  def isLocal(local: Local): Boolean = local match
    case _: Interaction => false
    case _: Action | _: RecursionCall | Skip => true
    case Sequence(localA, localB) => isLocal(localA) && isLocal(localB)
    case Parallel(localA, localB) => isLocal(localA) && isLocal(localB)
    case Choice  (localA, localB) => isLocal(localA) && isLocal(localB)
    case RecursionFixedPoint(_, localB) => isLocal(localB)
    case RecursionKleeneStar(localA)    => isLocal(localA)
  end isLocal

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

  def mkUnexpectedConstructMessage(protocol: Protocol): String =
    s"unexpected construct found in [$protocol]"
  end mkUnexpectedConstructMessage
end Protocol