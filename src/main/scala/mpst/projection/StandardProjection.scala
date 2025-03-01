package mpst.projection

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.utility.StructuralCongruence


object StandardProjection:
  def projectionWithParticipant(global: Global): Set[(Participant, Local)] =
    for participant <- getParticipants(global) yield
      projection(global)(using participant) match
        case Some(local) => participant -> StructuralCongruence(local)
        case _ => throw RuntimeException(s"projection undefined for [$participant] in [$global]\n")
  end projectionWithParticipant

  private def projection(global: Global)(using participant: Participant): Option[Local] = global match
    case interaction: Interaction if interaction.sender == participant && interaction.receiver != participant => Some(Send(interaction.sender, interaction.receiver, interaction.label))
    case interaction: Interaction if interaction.sender != participant && interaction.receiver == participant => Some(Recv(interaction.receiver, interaction.sender, interaction.label))
    case interaction: Interaction if interaction.sender != participant && interaction.receiver != participant => Some(Skip)
    case _ : RecursionCall | Skip => Some(global)
    case Sequence(globalA, globalB) =>
      for localA <- projection(globalA)
          localB <- projection(globalB)
      yield Sequence(localA, localB)
    case Parallel(globalA, globalB) =>
      for localA <- projection(globalA)
          localB <- projection(globalB)
      yield Parallel(localA, localB)
    case Choice(globalA, globalB) =>
      for localA <- projection(globalA)
          localB <- projection(globalB)
      yield Choice(localA, localB)
    case RecursionFixedPoint(variable, globalB) =>
      for localB <- projection(globalB) yield
        if getParticipants(localB).contains(participant) then RecursionFixedPoint(variable, localB) else Skip
    case RecursionKleeneStar(globalA) =>
      for localA <- projection(globalA) yield
        if getParticipants(localA).contains(participant) then RecursionKleeneStar(localA) else Skip
    case _ => None
  end projection
end StandardProjection