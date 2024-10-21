package mpst.operational_semantic

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
      nextAuxiliary(localsWithParticipant, pending)(using environment)
    end next

    private def nextAuxiliary[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Queue)(using environment: Map[Participant, Map[Variable, Local]]): Set[(A, Set[(Participant, Local)], Queue)] =
      for localWithParticipant <- localsWithParticipant yield
        nextEntry(localWithParticipant, pending).flatMap {
          (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
    end nextAuxiliary

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Queue)(using environment: Map[Participant, Map[Variable, Local]]): Set[(Action, (Participant, Local), Queue)] =
      def notBlocked(action: Action, pending: Queue): Boolean =
        action match
          case Send   (_, _, _, _) => true
          case Receive(receiver, sender, label, sort) => pending(sender, receiver).nonEmpty && pending(sender -> receiver).head == label
          case protocol => throw RuntimeException(s"unexpected Protocol found in [$protocol] where Action was expected")
      end notBlocked

      def nextPending(action: Action, pending: Queue): Queue =
        action match
          case Send   (sender, receiver, label, sort) => pending + ((sender -> receiver) -> (pending.getOrElse(sender -> receiver, Nil) :+ label))
          case Receive(receiver, sender, label, sort) => pending + ((sender -> receiver) -> pending(sender -> receiver).tail) // @ telmo - it passed the notBlocked test
          case protocol => throw RuntimeException(s"unexpected Protocol found in [$protocol] where Action was expected")
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)) if notBlocked(nextAction, pending) yield
        (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkCausal

  object NetworkMultiset:
    def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
      localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
    end accepting

    def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Map[Participant, Map[Variable, Local]]): Set[(A, Set[(Participant, Local)], Multiset[Action])] =
      nextAuxiliary(localsWithParticipant, pending)(using environment)
    end next

    private def nextAuxiliary[A >: Action](localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Map[Participant, Map[Variable, Local]]): Set[(A, Set[(Participant, Local)], Multiset[Action])] =
      for localWithParticipant <- localsWithParticipant yield
        nextEntry(localWithParticipant, pending).flatMap {
          (nextAction, nextLocalWithParticipant, nextPending) =>
            (nextAction, localsWithParticipant - localWithParticipant + nextLocalWithParticipant, nextPending)
        }
    end nextAuxiliary

    private def nextEntry(localWithParticipant: (Participant, Local), pending: Multiset[Action])(using environment: Map[Participant, Map[Variable, Local]]): Set[(Action, (Participant, Local), Multiset[Action])] =
      def notBlocked(action: Action, pending: Multiset[Action]): Boolean =
        action match
          case Send   (_, _, _, _) => true
          case Receive(receiver, sender, label, sort) => pending contains Send(sender, receiver, label, sort)
          case protocol => throw RuntimeException(s"unexpected Protocol found in [$protocol] where Action was expected")
      end notBlocked

      def nextPending(action: Action, pending: Multiset[Action]): Multiset[Action] =
        action match
          case Send   (sender, receiver, label, sort) => pending + Send(sender, receiver, label, sort)
          case Receive(receiver, sender, label, sort) => pending - Send(sender, receiver, label, sort)
          case protocol => throw RuntimeException(s"unexpected Protocol found in [$protocol] where Action was expected")
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)) if notBlocked(nextAction, pending) yield
        (nextAction, localWithParticipant._1 -> nextLocal, nextPending(nextAction, pending))
    end nextEntry
  end NetworkMultiset
end Network