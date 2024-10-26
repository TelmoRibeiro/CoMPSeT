package mpst.projection

import mpst.syntax.Protocol.*
import mpst.utility.StructuralCongruence

/* @ telmo
  IDEA:
    => replicate the projections of ST4MP + recursion
    => Plain Merge
  ISSUES:
    => check recursion
    => In order for us to successfully replicate ST4MP we have to check WellBranchedness after
  REVIEWED:
    => AFFIRMATIVE*
*/

object StandardProjection:
  def projectionWithParticipant(global: Global): Set[(Participant, Local)] =
    for participant <- getParticipants(global) yield
      projection(global)(using participant) match
        case Some(local) => participant -> StructuralCongruence(local)
        case _ => throw RuntimeException(s"projection undefined for participant [$participant] in [$global]\n")
  end projectionWithParticipant

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
      yield Choice(localA, localB)
    case RecursionFixedPoint(variable, globalB) =>
      for localB <- projection(globalB)
        yield RecursionFixedPoint(variable, localB)
    case _ => None
  end projection
end StandardProjection