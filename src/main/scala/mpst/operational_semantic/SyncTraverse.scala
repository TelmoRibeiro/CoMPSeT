package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Send, Receive}
import mpst.utility.Environment.Environment

/* @ telmo
  IDEA:
    => [[SyncTraverse]] models the communication progress between multiple [[Participant]]s.
    => Traverse supports synchronous message passing.
  ISSUES:
    None
  REVIEWED:
    => AFFIRMATIVE*
*/

object SyncTraverse:
  def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
    localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }
  end accepting

  def next[A >: Action](localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Receive])(using environment: Environment): Set[(A, Option[Receive], Set[(Participant, Local)])] =
    localsWithParticipant.flatMap( localWithParticipant =>
      nextEntry(localWithParticipant, pendingReceive).map{ case (nextAction, nextLocalWithParticipant, nextPendingReceive) =>
        (nextAction, nextPendingReceive, localsWithParticipant - localWithParticipant + nextLocalWithParticipant)
      }
    )
  end next

  private def nextEntry(localWithParticipant: (Participant, Local), pendingReceive: Option[Receive])(using environment: Environment): Set[(Action, (Participant, Local), Option[Receive])] =
    def notBlocked(action: Action, pendingReceive: Option[Receive]): Boolean = action match
      case _: Send => pendingReceive.isEmpty
      case receive: Receive => pendingReceive.isDefined && receive == pendingReceive.get
    end notBlocked

    def matchingReceive(nextSend: Send): Receive =
      Receive(nextSend.receiver, nextSend.sender, nextSend.label, nextSend.sort)
    end matchingReceive

    def nextPendingReceive(action: Action, pendingReceive: Option[Receive]): Option[Receive] = action match
      case send: Send => Some(matchingReceive(send))
      case _: Receive => None
    end nextPendingReceive

    for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
      if notBlocked(nextAction, pendingReceive)
    yield (nextAction, localWithParticipant._1 -> nextLocal, nextPendingReceive(nextAction, pendingReceive))
  end nextEntry
end SyncTraverse