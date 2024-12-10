package mpst.frontend.caos_wrapper

import mpst.operational_semantic.SyncTraverse
import mpst.syntax.Protocol.{Action, Local, Participant}
import mpst.utility.Environment.Environment

import caos.sos.SOS

/* @ telmo
 IDEA:
    => [[SyncTraverseWrapper]] wraps the [[SyncTraverse]] object to be used with [[CAOS]].
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object SyncTraverseWrapper:
  private type SyncState = (Set[(Participant, Local)], Environment)

  object Traverse extends SOS[Action, SyncState]:
    override def accepting(state: SyncState): Boolean =
      SyncTraverse.accepting(state._1)
    end accepting

    override def next[A >: Action](state: SyncState): Set[(A, SyncState)] =
      val nextAction -> nextLocalsWithParticipant = SyncTraverse.next(state._1)(using state._2)
      Set(nextAction -> (nextLocalsWithParticipant -> state._2))
    end next
  end Traverse
end SyncTraverseWrapper