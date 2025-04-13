package mpst.wellformedness

import mpst.operational_semantic.MPSTSemantic
import mpst.projection.StandardProjection
import mpst.syntax.Protocol.*
import mpst.utility.Environment.{SingleEnvironment, globalEnvironment}

/* @ telmo
  IDEA:
    => check well-formedness on branching
    => conditions:
      => there can be only one sender (selector)
      => the receivers on the left must match the receivers on the right and vice versa
      => there cannot be ambiguous actions on branches
      => the receivers must be readily available for the communication
  ISSUES:
    => couldn't receivesInReceive be relaxed and become the dual of receivesInSend?
    => shouldn't the 2nd rule become "there can only be one receiver?"
    => add full merge
  REVIEWED:
    => AFFIRMATIVE*
*/

object WellBranched:
  private def wellBranched(global: Global)(using environment: SingleEnvironment): Boolean = global match
    case _ : Interaction | _ : RecursionCall | Skip => true
    case Sequence(globalA, globalB) => wellBranched(globalA) && wellBranched(globalB)
    case Parallel(globalA, globalB) => wellBranched(globalA) && wellBranched(globalB)
    case Choice  (globalA, globalB) => globalA -> globalB match
      case Skip -> _ => throw RuntimeException(s"unguarded skip found on a Choice")
      case _ -> Skip => throw RuntimeException(s"unguarded skip found on a Choice")
      case _ => wellBranchedAuxiliary(globalA, globalB) && wellBranched(globalA) && wellBranched(globalB)
    case RecursionFixedPoint(_, globalB) => wellBranched(globalB)
    case RecursionKleeneStar(globalA)    => wellBranched(globalA)
    case _ => throw RuntimeException(mkUnexpectedConstructMessage(global))
  end wellBranched

  private def wellBranchedAuxiliary(globalA: Global, globalB: Global)(using environment: SingleEnvironment): Boolean =
    def nextActions(global: Global)(using environment: SingleEnvironment): Set[Action] = MPSTSemantic.next(global).map(_._1)

    def uniqueSelector(actions: Set[Action]): Participant =
      actions.collect {
        case sendAction: Send => sendAction.sender
      }.toList match
        case Nil => throw RuntimeException(s"no selector found in [$actions]")
        case selector :: Nil => selector
        case _ => throw RuntimeException(s"multiple selectors found in [$actions]")
    end uniqueSelector

    def receivesInSend(actions: Set[Action]): Set[(Participant, Label)] =
      actions.collect {
        case sendAction: Send => sendAction.receiver -> sendAction.label
      }
    end receivesInSend

    def receivesInReceive(global: Global, selector: Participant, receivesInSend: Set[(Participant, Label)])(using environment: SingleEnvironment): Set[(Participant, Label)] =
      for participant -> local <- StandardProjection.projectionWithParticipant(global)
        case Recv(_, `selector`, label) <- MPSTSemantic.next(local).map(_._1) if receivesInSend.contains(participant -> label)
      yield participant -> label
    end receivesInReceive

    def readilyReceived(receivesInSend: Set[(Participant, Label)], receivesInReceive: Set[(Participant, Label)], selector: Participant): Boolean =
      receivesInSend.forall {
        case entry @ receiver -> label if !receivesInReceive.contains(entry) =>
          throw RuntimeException(s"$selector > $receiver : $label cannot be readily received")
        case _ => true
      }
    end readilyReceived

    def noAmbiguity(receivesInReceiveA: Set[(Participant, Label)], receivesInReceiveB: Set[(Participant, Label)]): Boolean =
      receivesInReceiveA.intersect(receivesInReceiveB) match
        case intersection if intersection.nonEmpty =>
          throw RuntimeException(s"[$intersection] are ambiguous in both branches")
        case _ => true
    end noAmbiguity

    def matchingReceivers(receivesInReceiveA: Set[(Participant, Label)], receivesInReceiveB: Set[(Participant, Label)]): Boolean =
      receivesInReceiveA.forall {
        case receiver -> _ if !receivesInReceiveB.map(_._1).contains(receiver) =>
          throw RuntimeException(s"[$receiver] not matched in [$receivesInReceiveB]")
        case _ => true
      } && receivesInReceiveB.forall {
        case receiver -> _ if !receivesInReceiveA.map(_._1).contains(receiver) =>
          throw RuntimeException(s"[$receiver] not matched in [$receivesInReceiveA]")
        case _ => true
      }
    end matchingReceivers

    val nextActionsA = nextActions(globalA)
    val nextActionsB = nextActions(globalB)

    val selector = uniqueSelector(nextActionsA ++ nextActionsB)

    val receivesInSendA = receivesInSend(nextActionsA)
    val receivesInSendB = receivesInSend(nextActionsB)

    val receivesInReceiveA = receivesInReceive(globalA, selector, receivesInSendA)
    val receivesInReceiveB = receivesInReceive(globalB, selector, receivesInSendB)
    
    readilyReceived(receivesInSendA, receivesInReceiveA, selector) && readilyReceived(receivesInSendB, receivesInReceiveB, selector) && noAmbiguity(receivesInReceiveA, receivesInReceiveB) && matchingReceivers(receivesInReceiveA, receivesInReceiveB)
  end wellBranchedAuxiliary

  def apply(global: Global): Boolean =
    wellBranched(global)(using globalEnvironment(global))
  end apply
end WellBranched