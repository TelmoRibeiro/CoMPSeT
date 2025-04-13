package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Recv, Send, matchingAction}
import mpst.utility.Environment.Environment
import mpst.utility.Multiset

import scala.collection.immutable.Queue


object Network:
  object NetworkCausal:
    type ChannelQueue = Map[(Participant, Participant), Queue[Label]]

    def accepting(localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue): Boolean =
      localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) } && pending.isEmpty
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue)(using environment: Environment): Set[(A, Set[(Participant, Local)], ChannelQueue)] =
      localsWithParticipant.flatMap( localWithParticipant =>
        nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      )
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: ChannelQueue)(using environment: Environment): Set[(Action, (Participant, Local), ChannelQueue)] =
      def notBlocked(action: Action, pending: ChannelQueue): Boolean = action match
        case _: Send => true
        case recvAction: Recv =>
          pending.getOrElse(recvAction.sender -> recvAction.receiver, Queue.empty).headOption.contains(recvAction.label)
      end notBlocked

      def nextPending(action: Action, pending: ChannelQueue): ChannelQueue = action match
        case sendAction: Send =>
          val updatedQueue = pending.getOrElse(sendAction.sender -> sendAction.receiver, Queue.empty).enqueue(sendAction.label)
          pending + (sendAction.sender -> sendAction.receiver -> updatedQueue)
        case recvAction: Recv =>
          val key = recvAction.sender -> recvAction.receiver
          val updatedQueue = pending.getOrElse(key, Queue.empty).dequeue._2
          if updatedQueue.isEmpty then pending - key else pending + (key -> updatedQueue)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkCausal


  object NetworkNonCausal:
    def accepting(localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action]): Boolean =
      localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) } && pending.isEmpty
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Environment): Set[(A, Set[(Participant, Local)], Multiset[Action])] =
      localsWithParticipant.flatMap (
        localWithParticipant => nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      )
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Multiset[Action])(using environment: Environment): Set[(Action, (Participant, Local), Multiset[Action])] =
      def notBlocked(action: Action, pending: Multiset[Action]): Boolean = action match
        case _: Send => true
        case recvAction: Recv => pending contains matchingAction(recvAction)
      end notBlocked

      def nextPending(action: Action, pending: Multiset[Action]): Multiset[Action] = action match
        case sendAction: Send => pending + sendAction
        case recvAction: Recv => pending - matchingAction(recvAction)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkNonCausal
end Network