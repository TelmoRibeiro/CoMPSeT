package mpst.projection

import mpst.operational_semantic.MPSTSemantic.next
import mpst.syntax.Protocol.*
import mpst.utility.Environment.{SingleEnvironment, localsEnvironment}

object PlainMergeProjection:
  def projectionWithParticipant(global: Global): Set[(Participant, Local)] =
    val localsWithParticipant = StandardProjection.projectionWithParticipant(global)
    localsWithParticipant.map((participant, local) => plainMerge(local)(using localsWithParticipant.map(_._1))(using localsEnvironment(global)(participant)))
    localsWithParticipant
  end projectionWithParticipant

  private def plainMerge(local: Local)(using participants: Set[Participant])(using environment: SingleEnvironment): Boolean = local match
    case _: Action | _: RecursionCall | Skip => true
    case Sequence(localA, localB) => plainMerge(localA) && plainMerge(localB)
    case Parallel(localA, localB) => plainMerge(localA) && plainMerge(localB)
    case Choice  (localA, localB) => participants.forall(participant => plainMergeAuxiliary(localA, localB)(using participant)) && plainMerge(localA) && plainMerge(localB)
    case RecursionFixedPoint(_, localB) => plainMerge(localB)
    case RecursionKleeneStar(localA)    => plainMerge(localA)
    case _ => throw RuntimeException(mkUnexpectedConstructMessage(local))
  end plainMerge

  private def plainMergeAuxiliary(localA: Local, localB: Local)(using participant: Participant)(using environment: SingleEnvironment): Boolean =
    val participantsA = next(localA).flatMap((action, _) => getParticipants(action))
    val participantsB = next(localB).flatMap((action, _) => getParticipants(action))
    val continuationsA = next(localA).map(_._2)
    val continuationsB = next(localB).map(_._2)
    if !(participantsA union participantsB).contains(participant) && (continuationsA union continuationsB).size > 1 then
      throw RuntimeException(s"[Plain Merge] - projection undefined for [$participant] in [${Choice(localA, localB)}]")
    true
  end plainMergeAuxiliary
end PlainMergeProjection