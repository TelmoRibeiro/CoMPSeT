package mpst.frontend.auxiliary.wrappers

import caos.sos.SOS
import mpst.operational_semantic.SyncTraverse
import mpst.syntax.Protocol.{Action, Local, Participant, Recv}
import mpst.utility.Environment.Environment

/* @ telmo
 IDEA:
    => [[SyncTraverseWrapper]] wraps the [[SyncTraverse]] object to be used in [[CAOS]].
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object SyncEnvironmentWrapper:
  private type SyncState = (Set[(Participant, Local)], Option[Recv], Environment)

  object SyncTraverseWrapper extends SOS[Action, SyncState]:
    override def accepting(state: SyncState): Boolean =
      SyncTraverse.accepting(state._1)
    end accepting

    override def next[A >: Action](state: SyncState): Set[(A, SyncState)] =
      for (nextAction, nextPendingReceive, nextLocalsWithParticipant) <- SyncTraverse.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPendingReceive, state._3)
    end next
  end SyncTraverseWrapper
end SyncEnvironmentWrapper