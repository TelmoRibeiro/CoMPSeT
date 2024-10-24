package mpst.operational_semantic

import mpst.syntax.Protocol.*
import mpst.syntax.Type.{Local, Participant, Action, Environment}
import mpst.utilities.Environment.{localEnv, singleLocalEnv}

/* @ telmo
  IDEA:
    => [[SyncTraverse]] models the communication progress between multiple [[Participant]]s.
    => Traverse supports synchronous message passing.
  ISSUES:
    => nextSend and nextReceive may be evolving in multiple evaluation contexts
    => ambiguity may be too restrictive of a clause (sendAction)
  REVIEWED:
    => AFFIRMATIVE*
*/

object SyncTraverse:
  def accepting(localsWithParticipant: Set[(Participant, Local)]): Boolean =
    localsWithParticipant.forall(localWithParticipant => MPSTSemantic.accepting(localWithParticipant._2))
  end accepting

  def next[A >: Action](localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): (A, Set[(Participant, Local)]) =
    val send = sendAction(localsWithParticipant)
    val sendTraverse = localsWithParticipant.flatMap { localWithParticipant =>
      nextSend(localWithParticipant, send)
      }
    val receiveTraverse = sendTraverse.flatMap { localWithParticipant =>
      nextReceive(localWithParticipant, send)
    }
    send -> receiveTraverse
  end next

  private def sendAction(localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): Action =
    val toSend = localsWithParticipant.flatMap { localWithParticipant =>
      for nextAction -> nextLocal <- MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1))
          if nextAction match
            case Send(_, _, _, _) => true
            case _ => false
      yield nextAction
    }.toList
    toSend match
      case Nil => throw RuntimeException(s"no send action found")
      case send :: Nil => send
      case _ => throw RuntimeException(s"possible ambiguity in [$toSend] found")
  end sendAction

  private def nextSend(localWithParticipant: (Participant, Local), sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
    MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)).map {
      case nextAction -> nextLocal if nextAction == sendAction => localWithParticipant._1 -> nextLocal
      case _ => localWithParticipant
    }
  end nextSend

  private def nextReceive(localWithParticipant: (Participant, Local), sendAction: Action)(using environment: Environment): Set[(Participant, Local)] =
    MPSTSemantic.next(localWithParticipant._2)(using environment(localWithParticipant._1)).map {
      case nextAction -> nextLocal
        if nextAction match
          case Receive(receiver, sender, label, sort) => sendAction match
            case Send(sender, receiver, label, sort) => true
            case _ => false
          case _ => false
      => localWithParticipant._1 -> nextLocal
      case _ => localWithParticipant
    }
  end nextReceive
end SyncTraverse