package mpst.operational_semantic

import mpst.operational_semantic.Network.NetworkCausal.nextEntry
import mpst.operational_semantic.Network.NetworkMultiset.nextEntry
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.{Action, Local, Participant, Queue, Variable}
import mpst.utilities.Multiset

/* @ telmo
  IDEA:
    => asynchronous network with causal and multiset options
  ISSUES:
    => environment is applied to global (only) - need to check what I meant by this
  REVIEWED:
    => NEGATIVE
*/

object Network:
  object NetworkCausal:
    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Queue)(using environment: Map[Participant, Map[Variable, Local]]): Set[(A, Set[(Participant, Local)], Queue)] =
      localsWithParticipant.flatMap {
        localWithParticipant => nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      }
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Queue)(using environment: Map[Participant, Map[Variable, Local]]): Set[(Action, (Participant, Local), Queue)] =
      def notBlocked(action: Action, pending: Queue): Boolean = action match
        case Send   (_, _, _, _) => true
        case Receive(receiver, sender, label, sort) => pending(sender, receiver).nonEmpty && pending(sender -> receiver).head == label
        case _ => throw RuntimeException(s"unexpected Protocol found in [$action] where [Action] was expected")
      end notBlocked

      def nextPending(action: Action, pending: Queue): Queue = action match
        case Send   (sender, receiver, label, sort) => pending + ((sender -> receiver) -> (pending.getOrElse(sender -> receiver, Nil) :+ label))
        case Receive(receiver, sender, label, sort) => pending + ((sender -> receiver) -> pending(sender -> receiver).tail)
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

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Map[Participant, Map[Variable, Local]]): Set[(A, Set[(Participant, Local)], Multiset[Action])] =
      localsWithParticipant.flatMap {
        localWithParticipant => nextEntry(localWithParticipant, pending).map {
          case (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
      }
    end next

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Multiset[Action])(using environment: Map[Participant, Map[Variable, Local]]): Set[(Action, (Participant, Local), Multiset[Action])] =
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