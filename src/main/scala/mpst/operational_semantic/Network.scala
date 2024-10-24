package mpst.operational_semantic

import mpst.syntax.Protocol.*
import mpst.syntax.Type.{Local, Participant, Action, ChannelQueue, Environment}
import mpst.utilities.Multiset

/* @ telmo
  IDEA:
    => [[Network]] models the communication progress between multiple [[Participant]]s.
    => The network supports both [[Causal]] ([[ChannelQueue]] - ordered) and [[NonCausal]] ([[Multiset]] - unordered) message passing.
  ISSUES:
    => check if the queue is session-wise or channel-wise
  REVIEWED:
    => AFFIRMATIVE*
*/

object Network:
  object NetworkCausal:
    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue)(using environment: Environment): Set[(A, Set[(Participant, Local)], ChannelQueue)] =
      localsWithParticipant.flatMap {
        localWithParticipant => nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      }
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: ChannelQueue)(using environment: Environment): Set[(Action, (Participant, Local), ChannelQueue)] =
      def notBlocked(action: Action, pending: ChannelQueue): Boolean = action match
        case Send   (_, _, _, _) => true
        case Receive(receiver, sender, label, sort) =>
          pending.headOption.exists {
            case (headSender, headReceiver, headLabel) => headSender == sender && headReceiver == receiver && headLabel == label
          }
        case _ => throw RuntimeException(s"unexpected Protocol found in [$action] where [Action] was expected")
      end notBlocked

      def nextPending(action: Action, pending: ChannelQueue): ChannelQueue = action match
        case Send   (sender, receiver, label, sort) => pending.enqueue((sender, receiver, label))
        case Receive(receiver, sender, label, sort) => pending.dequeue._2
        case _ => throw RuntimeException(s"unexpected Protocol found in [$action] where [Action] was expected")
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
          if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkCausal

  object NetworkMultiset:
    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
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
        case _ => throw RuntimeException(s"unexpected Protocol found in [$action] where [Action] was expected")
      end notBlocked

      def nextPending(action: Action, pending: Multiset[Action]): Multiset[Action] = action match
        case Send   (sender, receiver, label, sort) => pending + Send(sender, receiver, label, sort)
        case Receive(receiver, sender, label, sort) => pending - Send(sender, receiver, label, sort)
        case _ => throw RuntimeException(s"unexpected Protocol found in [$action] where [Action] was expected")
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
          if notBlocked(nextAction, pending)
      yield (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkMultiset
end Network