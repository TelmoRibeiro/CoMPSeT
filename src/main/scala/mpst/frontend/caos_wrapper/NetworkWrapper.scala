package mpst.frontend.caos_wrapper

import mpst.operational_semantic.Network
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.syntax.Protocol.{Action, Local, Participant}
import mpst.utility.Environment.Environment
import mpst.utility.Multiset

import caos.sos.SOS

/* @ telmo
  IDEA:
    => [[NetworkWrapper]] wraps the [[Network]] object to be used with [[CAOS]].
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object NetworkWrapper:
    private type CSState = (Set[(Participant, Local)], ChannelQueue, Environment)

    object NetworkCausal extends SOS[Action, CSState]:
      override def accepting(state: CSState): Boolean =
        Network.NetworkCausal.accepting(state._1)
      end accepting

      override def next[A >: Action](state: CSState): Set[(A, CSState)] =
        for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkCausal.next(state._1, state._2)(using state._3) yield
          nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
      end next
    end NetworkCausal

    private type MSState = (Set[(Participant, Local)], Multiset[Action], Environment)

    object NetworkMultiset extends SOS[Action, MSState]:
      override def accepting(state: MSState): Boolean =
        Network.NetworkMultiset.accepting(state._1)
      end accepting

      override def next[A >: Action](state: MSState): Set[(A, MSState)] =
        for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkMultiset.next(state._1, state._2)(using state._3) yield
          nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
      end next
    end NetworkMultiset
end NetworkWrapper