package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Recv, Send, matchingAction}
import mpst.utility.Environment.Environment


object SyncTraverse:
  def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean = localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }

  def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv])(using environment: Environment): Set[(A, Option[Recv], Set[(Participant, Local)])] =
    localsWithParticipant.flatMap( localWithParticipant =>
      nextEntry(localWithParticipant, localsWithParticipant - localWithParticipant, pendingReceive).map {
        case (nextAction, nextLocalWithParticipant, nextPendingReceive) =>
          (nextAction, nextPendingReceive, localsWithParticipant - localWithParticipant + nextLocalWithParticipant)
      }
    )
  end next

  private def nextEntry(localWithParticipant: (Participant, Local), remainingLocalsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv])(using environment: Environment): Set[(Action, (Participant, Local), Option[Recv])] =
    def notBlocked(action: Action, remainingLocalsWithParticipants: Set[(Participant, Local)], pendingReceive: Option[Recv]): Boolean = action match
      case sendAction: Send =>
        pendingReceive.isEmpty && remainingLocalsWithParticipant.flatMap {
          case participant -> local => MPSTSemantic.next(local)(using environment(participant))
        }.exists(_._1 == matchingAction(sendAction))
      case recvAction: Recv => pendingReceive.isDefined && recvAction == pendingReceive.get
    end notBlocked

    def nextPendingReceive(action: Action, pendingReceive: Option[Recv]): Option[Recv] = action match
      case sendAction: Send => Some(matchingAction(sendAction).asInstanceOf[Recv])
      case _: Recv => None
    end nextPendingReceive

    for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
      if notBlocked(nextAction, remainingLocalsWithParticipant, pendingReceive)
    yield (nextAction, localWithParticipant._1 -> nextLocal, nextPendingReceive(nextAction, pendingReceive))
  end nextEntry
end SyncTraverse