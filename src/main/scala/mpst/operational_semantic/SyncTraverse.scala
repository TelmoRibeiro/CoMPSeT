package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Send, Receive}
import mpst.utility.Environment.Environment

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

  def next[A >: Action](localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): Set[(A, Set[(Participant, Local)])] =
    nextSends(localsWithParticipant).map( nextSend =>
      nextSend -> traverse(localsWithParticipant)(using nextSend)
    )
  end next

  private def nextSends(localsWithParticipant: Set[(Participant, Local)])(using environment: Environment): Set[Send] =
    localsWithParticipant
    .flatMap{ case participant -> local => MPSTSemantic.next(local)(using environment(participant)) }
    .collect{ case (nextSend @ _: Send) -> _ => nextSend }
  end nextSends

  private def matchingReceive(nextSend: Send): Receive =
    Receive(nextSend.receiver, nextSend.sender, nextSend.label, nextSend.sort)
  end matchingReceive

  private def traverse(localsWithParticipant: Set[(Participant, Local)])(using nextSend: Send)(using environment: Environment): Set[(Participant, Local)] =
    localsWithParticipant.flatMap { case participant -> local =>
      MPSTSemantic.next(local)(using environment(participant)).collect {
        case (`nextSend`, nextLocal) => participant -> nextLocal
        case (nextRecv,   nextLocal) if nextRecv == matchingReceive(nextSend) => participant -> nextLocal
      }
    }
  end traverse
end SyncTraverse