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
        case _ => throw RuntimeException(s"projection undefined for participant [$participant] in [$global]\n")
  end projectionWithParticipant

  private def plainMerge(localA: Local, localB: Local): Option[Local] =
    localA -> localB match
      case Sequence(Skip, contLocalA) -> Sequence(Skip, contLocalB) if contLocalA == contLocalB =>
        Some(contLocalA)
      case Sequence(Skip, contLocalA) -> Sequence(Skip, contLocalB) =>
        throw RuntimeException(s"plain merge failed for [$localA] and [$localB]")
      case _ => Some(Choice(localA, localB))
  end plainMerge

  private def fullMerge(localA: Local, localB: Local): Option[Local] =
    ???
  end fullMerge

  private def projection(global: Global)(using participant: Participant): Option[Local] = global match
    case Interaction(`participant`, receiver, label, sort) =>
      Some(Send(participant, receiver, label, sort))
    case Interaction(sender, `participant`, label, sort) =>
      Some(Receive(participant, sender, label, sort))
    case _ : Interaction =>
      Some(Skip)
    case _ : RecursionCall =>
      Some(global)
    case Skip =>
      Some(Skip)
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
          localC <- plainMerge(localA, localB)
      yield
        localC
    case RecursionFixedPoint(variable, globalB) =>
      for localB <- projection(globalB) yield
        if getParticipants(localB).contains(participant) then RecursionFixedPoint(variable, localB) else Skip
    case RecursionKleeneStar(globalA) =>
      for localA <- projection(globalA) yield
        if getParticipants(localA).contains(participant) then RecursionKleeneStar(localA) else Skip
    case _ => None
  end projection
end StandardProjection