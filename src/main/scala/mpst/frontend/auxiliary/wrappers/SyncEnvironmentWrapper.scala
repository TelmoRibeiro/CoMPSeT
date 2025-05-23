package mpst.frontend.auxiliary.wrappers

import caos.sos.SOS
import mpst.operational_semantic.SyncTraverse
import mpst.syntax.Protocol.{Action, Local, Participant, Recv}
import mpst.utility.Environment.Environment


object SyncEnvironmentWrapper:
  type SyncState = (Set[(Participant, Local)], Option[Recv], Environment)

  object SyncTraverseWrapper extends SOS[Action, SyncState]:
    override def accepting(state: SyncState): Boolean =
      SyncTraverse.accepting(state._1, state._2)
    end accepting

    override def next[A >: Action](state: SyncState): Set[(A, SyncState)] =
      for (nextAction, nextPendingReceive, nextLocalsWithParticipant) <- SyncTraverse.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPendingReceive, state._3)
    end next
  end SyncTraverseWrapper
end SyncEnvironmentWrapper