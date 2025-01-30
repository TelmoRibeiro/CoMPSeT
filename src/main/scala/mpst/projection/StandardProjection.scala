package mpst.projection

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.utility.StructuralCongruence

/* @ telmo
  IDEA:
    => replicate the projections of VGI
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object StandardProjection:
  def projectionWithParticipant(global: Global): Set[(Participant, Local)] =
    for participant <- getParticipants(global) yield
      projection(global)(using participant) match
        case Some(local) => participant -> StructuralCongruence(local)
        case _ => throw RuntimeException(s"projection undefined for [$participant] in [$global]\n")
  end projectionWithParticipant

  private def projection(global: Global)(using participant: Participant): Option[Local] = global match
    case Interaction(`participant`, receiver, label, sort) if `participant` != receiver => Some(Send(participant, receiver, label, sort))
    case Interaction(sender, `participant`, label, sort)   if `participant` != sender   => Some(Receive(participant, sender, label, sort))
    case Interaction(sender, receiver, label, sort)        if `participant` != sender && `participant` != receiver => Some(Skip)
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