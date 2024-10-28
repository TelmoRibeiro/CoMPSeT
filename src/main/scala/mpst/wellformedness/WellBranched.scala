package mpst.wellformedness

import mpst.operational_semantic.MPSTSemantic
import mpst.syntax.Protocol.*
import mpst.utility.Environment.{SingleEnvironment, globalEnvironment}

/* @ telmo
  IDEA:
    => check well-formedness on branching
  ISSUES:
    => add full merge
  REVIEWED:
    => NEGATIVE
*/

object WellBranched:
  private def wellBranched(global: Global)(using environment: SingleEnvironment): Boolean = global match
    case _ : Interaction | _ : RecursionCall | Skip => true
    case Sequence(globalA, globalB) =>
      wellBranched(globalA) && wellBranched(globalB)
    case Parallel(globalA, globalB) =>
      wellBranched(globalA) && wellBranched(globalB)
    case Choice(globalA, globalB) =>
      wellBranchedAuxiliary(globalA, globalB) && wellBranched(globalA) && wellBranched(globalB)
    case RecursionFixedPoint(_, globalB) =>
      wellBranched(globalB)
    case _ => throw RuntimeException(s"unexpected local type found in [$global]")
  end wellBranched

  private def wellBranchedAuxiliary(globalA: Global, globalB: Global)(using environment: SingleEnvironment): Boolean =
    def nextActions(global: Global)(using environment: SingleEnvironment): Set[Action] =
      for action -> _ <- MPSTSemantic.next(global) yield action
    end nextActions

    def receivesInSend(actions: Set[Action]): Set[(Participant, Label)] =
      actions.collect {
        case Send(_, receiver, label, _) => receiver -> label
      }
    end receivesInSend

    def receivesInReceive(actions: Set[Action], receivesInSend: Set[(Participant, Label)])(using environment: SingleEnvironment): Set[(Participant, Label)] =
      actions.collect {
        case Receive(receiver, _, label, _) if receivesInSend.contains(receiver -> label) => receiver -> label
      }
    end receivesInReceive

    def uniqueSelector(actions: Set[Action]): Boolean =
      actions.collect {
        case Send(selector, _, _, _) => selector
      }.toList match
        case Nil => throw RuntimeException(s"no selector found in [$actions]")
        case selector :: Nil => true
        case _ => throw RuntimeException(s"multiple selectors found in [$actions]")
    end uniqueSelector

    def noAmbiguity(receivesA: Set[(Participant, Label)], receivesB: Set[(Participant, Label)]): Boolean =
      receivesA.intersect(receivesB) match
        case intersection if intersection.nonEmpty =>
          throw RuntimeException(s"ambiguous actions found in [$intersection]")
        case _ => true
    end noAmbiguity

    def matchingReceives(receivesA: Set[(Participant, Label)], receivesB: Set[(Participant, Label)]): Boolean =
      receivesA.forall {
        case receiver -> _ if !receivesB.map(_._1).contains(receiver) =>
          throw RuntimeException(s"[$receiver] not matched in [$receivesB]")
        case _ => true
      } && receivesB.forall {
        case receiver -> _ if !receivesA.map(_._1).contains(receiver) =>
          throw RuntimeException(s"[$receiver] not matched in [$receivesA]")
        case _ => true
      }
    end matchingReceives

    val nextActionsA = nextActions(globalA)
    val nextActionsB = nextActions(globalB)

    val receivesA = receivesInReceive(nextActionsA, receivesInSend(nextActionsA))
    val receivesB = receivesInReceive(nextActionsB, receivesInSend(nextActionsB))
    
    uniqueSelector(nextActionsA ++ nextActionsB) && noAmbiguity(receivesA, receivesB) && matchingReceives(receivesA, receivesB)
  end wellBranchedAuxiliary

  def apply(global: Global): Boolean = wellBranched(global)(using globalEnvironment(global))
end WellBranched