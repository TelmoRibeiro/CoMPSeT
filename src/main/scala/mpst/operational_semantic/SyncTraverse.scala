package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Send, Receive}
import mpst.utilities.Environment.Environment

/* @ telmo
  IDEA:
    => [[SyncTraverse]] models the communication progress between multiple [[Participant]]s.
    => Traverse supports synchronous message passing.
  ISSUES:
    => nextSend and nextReceive may be evolving in multiple evaluation contexts
    => ambiguity may be too restrictive of a clause (sendAction)
      either the projection rules restrict the possible sendActions
      or this condition must be relaxed
  REVIEWED:
    => AFFIRMATIVE*
*/

object SyncTraverse:
  def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
    localsWithParticipant.forall{ case _ -> local => MPSTSemantic.accepting(local) }
  end accepting

  def next[A >: Action](localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): (A, Set[(Participant, Local)]) =
    val send = sendAction(localsWithParticipant)
    send -> receiveTraverse(sendTraverse(localsWithParticipant, send), send)
  end next

  private def sendAction(localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): Action =
    localsWithParticipant.flatMap {
      case participant -> local =>
        MPSTSemantic.next(local)(using environment(participant)).collect {
          case (nextAction @ Send(_, _, _, _)) -> _ => nextAction
        }
    }.toList match
      case Nil         => throw RuntimeException(s"no send action found")
      case send :: Nil => send
      case sendList    => throw RuntimeException(s"possible ambiguity in [$sendList] found")
  end sendAction

  private def sendTraverse(localsWithParticipant: Set[(Participant, Local)], sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
    def nextSend(localWithParticipant: (Participant, Local), sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
      MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)).collect {
        case `sendAction` -> nextLocal => localWithParticipant._1 -> nextLocal
      }
    end nextSend

    localsWithParticipant.flatMap{ localWithParticipant =>
      nextSend(localWithParticipant, sendAction)
    }
  end sendTraverse

  private def receiveTraverse(localsWithParticipant: Set[(Participant, Local)], sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
    def nextReceive(localWithParticipant: (Participant, Local), sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
      MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)).collect {
        case Receive(receiver, sender, label, sort) -> nextLocal
          if sendAction == Send(sender, receiver, label, sort) =>
          localWithParticipant._1 -> nextLocal
      }
    end nextReceive

    localsWithParticipant.flatMap{ localWithParticipant =>
      nextReceive(localWithParticipant, sendAction)
    }
  end receiveTraverse
end SyncTraverse