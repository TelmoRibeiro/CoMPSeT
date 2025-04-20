package mpst.frontend.auxiliary.wrappers

import caos.sos.SOS
import mpst.operational_semantic.Network
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.syntax.Protocol.*
import mpst.utility.Environment.Environment
import mpst.utility.Multiset


object NetworkWrapper:
  type CausalState = (Set[(Participant, Local)], ChannelQueue, Environment)

  object NetworkCausal extends SOS[Action, CausalState]:
    override def accepting(state: CausalState): Boolean =
      Network.NetworkCausal.accepting(state._1, state._2)
    end accepting

    override def next[A >: Action](state: CausalState): Set[(A, CausalState)] =
      for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkCausal.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
    end next
  end NetworkCausal

  
  type NonCausalState = (Set[(Participant, Local)], Multiset[Action], Environment)

  object NetworkNonCausal extends SOS[Action, NonCausalState]:
    override def accepting(state: NonCausalState): Boolean =
      Network.NetworkNonCausal.accepting(state._1, state._2)
    end accepting

    override def next[A >: Action](state: NonCausalState): Set[(A, NonCausalState)] =
      for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkNonCausal.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
    end next
  end NetworkNonCausal
end NetworkWrapper