package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Send, Receive}
import mpst.utility.Environment.Environment
import mpst.utility.Multiset

import scala.collection.immutable.Queue

/* @ telmo
  IDEA:
    => [[Network]] models the communication progress between multiple [[Participant]]s.
    => The network supports both [[Causal]] ([[ChannelQueue]] - ordered) and [[NonCausal]] ([[Multiset]] - unordered) message passing.
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object Network:
  object NetworkCausalSession:
    type SessionQueue = Queue[(Participant, Participant, Label)]

    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: SessionQueue)(using environment: Environment): Set[(A, Set[(Participant, Local)], SessionQueue)] =
      localsWithParticipant.flatMap( localWithParticipant =>
        nextEntry(localWithParticipant, pending).map { case (nextAction, nextLocalWithParticipant, nextPending) =>
          (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      )
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: SessionQueue)(using environment: Environment): Set[(Action, (Participant, Local), SessionQueue)] =
      def notBlocked(action: Action, pending: SessionQueue): Boolean = action match
        case _: Send => true
        case Receive(receiver, sender, label, _) => pending.headOption.contains((sender, receiver, label))
      end notBlocked

      def nextPending(action: Action, pending: SessionQueue): SessionQueue = action match
        case Send   (sender, receiver, label, _) => pending.enqueue((sender, receiver, label))
        case Receive(receiver, sender, label, _) => if pending.headOption.contains((sender, receiver, label))
          then pending.dequeue._2
          else pending
        end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
          if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkCausalSession

  object NetworkCausal:
    type ChannelQueue = Map[(Participant, Participant), Queue[Label]]

    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue)(using environment: Environment): Set[(A, Set[(Participant, Local)], ChannelQueue)] =
      localsWithParticipant.flatMap( localWithParticipant =>
        nextEntry(localWithParticipant, pending).map { case (nextAction, nextLocalWithParticipant, nextPending) =>
          (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      )
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: ChannelQueue)(using environment: Environment): Set[(Action, (Participant, Local), ChannelQueue)] =
      def notBlocked(action: Action, pending: ChannelQueue): Boolean = action match
        case _: Send => true
        case Receive(receiver, sender, label, _) =>
          pending.getOrElse(sender -> receiver, Queue.empty).headOption.contains(label)
      end notBlocked

      def nextPending(action: Action, pending: ChannelQueue): ChannelQueue = action match
        case Send   (sender, receiver, label, sort) =>
          val updatedQueue = pending.getOrElse(sender -> receiver, Queue.empty).enqueue(label)
          pending + (sender -> receiver -> updatedQueue)
        case Receive(receiver, sender, label, sort) =>
          val updatedQueue = pending.getOrElse(sender -> receiver, Queue.empty).dequeue._2
          pending + (sender -> receiver -> updatedQueue)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkCausal

  object NetworkMultiset:
    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Environment): Set[(A, Set[(Participant, Local)], Multiset[Action])] =
      localsWithParticipant.flatMap {
        localWithParticipant => nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      }
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Multiset[Action])(using environment: Environment): Set[(Action, (Participant, Local), Multiset[Action])] =
      def notBlocked(action: Action, pending: Multiset[Action]): Boolean = action match
        case Send   (_, _, _, _) => true
        case Receive(receiver, sender, label, sort) => pending contains Send(sender, receiver, label, sort)
      end notBlocked

      def nextPending(action: Action, pending: Multiset[Action]): Multiset[Action] = action match
        case Send   (sender, receiver, label, sort) => pending + Send(sender, receiver, label, sort)
        case Receive(receiver, sender, label, sort) => pending - Send(sender, receiver, label, sort)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkMultiset
end Network