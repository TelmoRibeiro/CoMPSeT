package mpst.frontend.caos_wrapper

import mpst.operational_semantic.Network
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.syntax.Protocol.*
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
  private type CausalState = (Set[(Participant, Local)], ChannelQueue, Environment)

  object NetworkCausal extends SOS[Action, CausalState]:
    override def accepting(state: CausalState): Boolean = Network.NetworkCausal.accepting(state._1)

    override def next[A >: Action](state: CausalState): Set[(A, CausalState)] =
      for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkCausal.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
    end next
  end NetworkCausal

  
  private type NonCausalState = (Set[(Participant, Local)], Multiset[Action], Environment)

  object NetworkMultiset extends SOS[Action, NonCausalState]:
    override def accepting(state: NonCausalState): Boolean = Network.NetworkMultiset.accepting(state._1)

    override def next[A >: Action](state: NonCausalState): Set[(A, NonCausalState)] =
      for (nextAction, nextLocalsWithParticipant, nextPending) <- Network.NetworkMultiset.next(state._1, state._2)(using state._3) yield
        nextAction -> (nextLocalsWithParticipant, nextPending, state._3)
    end next
  end NetworkMultiset
end NetworkWrapper