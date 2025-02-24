package mpst.projection

import mpst.operational_semantic.MPSTSemantic.next
import mpst.projection.StandardProjection
import mpst.syntax.Protocol.*
import mpst.utility.Environment.{SingleEnvironment, localsEnvironment}

object FullMergeProjection:
  def projectionWithParticipant(global: Global): Set[(Participant, Local)] =
    val localsWithParticipant = StandardProjection.projectionWithParticipant(global)
    localsWithParticipant.map((participant, local) => fullMerge(local)(using localsWithParticipant.map(_._1))(using localsEnvironment(global)(participant)))
    localsWithParticipant
  end projectionWithParticipant

  private def fullMerge(local: Local)(using participants: Set[Participant])(using environment: SingleEnvironment): Boolean = local match
    case _: Send | _: Receive | _: RecursionCall | Skip => true
    case Sequence(localA, localB) =>
      fullMerge(localA) && fullMerge(localB)
    case Parallel(localA, localB) =>
      fullMerge(localA) && fullMerge(localB)
    case Choice(localA, localB) =>
      participants.forall(participant => fullMergeAuxiliary(localA, localB)(using participant)) && fullMerge(localA) && fullMerge(localB)
    case RecursionFixedPoint(_, localB) =>
      fullMerge(localB)
    case RecursionKleeneStar(localA) =>
      fullMerge(localA)
    case _ => throw RuntimeException(s"unexpected construct found in [$local]")
  end fullMerge

  private def fullMergeAuxiliary(localA: Local, localB: Local)(using participant: Participant)(using environment: SingleEnvironment): Boolean =
    val participantsA = next(localA).flatMap((action, _) => getParticipants(action))
    val participantsB = next(localB).flatMap((action, _) => getParticipants(action))
    val continuationsA = next(localA).map(_._2)
    val continuationsB = next(localB).map(_._2)
    if !(participantsA union participantsB).contains(`participant`) && (continuationsA union continuationsB).size > 1 then
      println(s"participant: ${`participant`}")
      println(s"continuationA: $continuationsA")
      println(s"continuationB: $continuationsB")
      println()
      isMergeable(continuationsA, continuationsB)(participant)(environment) && isMergeable(continuationsB, continuationsA)(participant)(environment)
    else true
  end fullMergeAuxiliary

  private def isMergeable(continuationsA: Set[Local], continuationsB: Set[Local])(participant: Participant)(environment: SingleEnvironment): Boolean =
    continuationsA.flatMap{ continuationA =>
      next(continuationA)(using environment).flatMap{ case actionA -> nextContinuationA => actionA match
        case sendA @ Send(`participant`, receiver, label, _) =>
          continuationsB.flatMap { continuationB =>
            next(continuationB)(using environment).flatMap {
              case actionB -> nextContinuationB => actionB match
                case receiveB: Receive =>
                  throw RuntimeException(s"[Full Merge] - could not merge [$sendA] with [$receiveB]")
                case sendB: Send if sendA.sender != sendB.sender || sendA.receiver != sendB.receiver || sendA.label != sendB.label =>
                  throw RuntimeException(s"[Full Merge] - could not merge [$sendA] with [$sendB]")
                case _ => Set.empty
            }
          }
        case sendA @ Send(sender, `participant`, label, _) =>
          continuationsB.flatMap { continuationB =>
            next(continuationB)(using environment).flatMap {
              case actionB -> nextContinuationsB => actionB match
                case receiveB: Receive =>
                  throw RuntimeException(s"[Full Merge] - could not merge [$sendA] with [$receiveB]")
                case sendB: Send if sendA.receiver == sendB.receiver && sendA.sender == sendB.sender && sendA.label == sendB.label && nextContinuationsB == nextContinuationsB =>
                  throw RuntimeException(s"[Full Merge] - could not merge [$sendA] with [$sendB]")
                case _ => Set.empty
            }
          }
        case receiveA @ Receive(`participant`, sender, label, _) =>
          continuationsB.flatMap { continuationB =>
            next(continuationB)(using environment).flatMap { case actionB -> nextContinuationB => actionB match
              case sendB: Send =>
                throw RuntimeException(s"[Full Merge] - could not merge [$receiveA] with [$sendB]")
              case receiveB: Receive if receiveA.receiver == receiveB.receiver && receiveA.sender == receiveB.sender && receiveA.label == receiveB.label && nextContinuationA == nextContinuationB =>
                throw RuntimeException(s"[Full Merge] - could not merge [$receiveA] with [$receiveB]")
              case _ => Set.empty
            }
          }
        case receiveA @ Receive(receiver, `participant`, label, _) =>
          continuationsB.flatMap { continuationA =>
            next(continuationA)(using environment).flatMap { case actionB -> nextContinuationB => actionB match
              case sendB: Send =>
                throw RuntimeException(s"[Full Merge] - could not merge [$receiveA] with [$sendB]")
              case receiveB: Receive if receiveA.sender != receiveB.sender || receiveA.receiver != receiveB.receiver || receiveA.label != receiveB.label =>
                throw RuntimeException(s"[Full Merge] - could not merge [$receiveA] with [$receiveB")
              case _ => Set.empty
            }
          }
        case _ => Set.empty
      }
    }
    true
  end isMergeable
end FullMergeProjection