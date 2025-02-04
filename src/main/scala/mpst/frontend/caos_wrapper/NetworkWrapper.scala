package mpst.frontend.caos_wrapper

import mpst.operational_semantic.Network
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.operational_semantic.Network.NetworkCausalSession.SessionQueue
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
    private type CausalChannelState = (Set[(Participant, Local)], ChannelQueue, Environment)

    object NetworkCausal extends SOS[Action, CausalChannelState]:
      override def accepting(state: CausalChannelState): Boolean =
        Network.NetworkCausal.accepting(state._1)
      end accepting

      override def next[A >: Action](state: CausalChannelState): Set[(A, CausalChannelState)] =
        for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkCausal.next(state._1, state._2)(using state._3) yield
          nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
      end next
    end NetworkCausal

    private type CausalSessionState = (Set[(Participant, Local)], SessionQueue, Environment)

    object NetworkSessionCausal extends SOS[Action, CausalSessionState]:
      override def accepting(state: CausalSessionState): Boolean =
        Network.NetworkCausalSession.accepting(state._1)
      end accepting

      override def next[A >: Action](state: CausalSessionState): Set[(A, CausalSessionState)] =
        for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkCausalSession.next(state._1, state._2)(using state._3) yield
          nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
      end next
    end NetworkSessionCausal

    private type NonCausalState = (Set[(Participant, Local)], Multiset[Action], Environment)

    object NetworkMultiset extends SOS[Action, NonCausalState]:
      override def accepting(state: NonCausalState): Boolean =
        Network.NetworkMultiset.accepting(state._1)
      end accepting

      override def next[A >: Action](state: NonCausalState): Set[(A, NonCausalState)] =
        for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkMultiset.next(state._1, state._2)(using state._3) yield
          nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
      end next
    end NetworkMultiset
end NetworkWrapper