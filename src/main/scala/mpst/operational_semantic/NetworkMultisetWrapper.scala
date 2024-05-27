package mpst.operational_semantic

import caos.sos.SOS

import mpst.syntax.Type.*

import mpst.syntax.Protocol

import mpst.operational_semantic.Network.NetworkMultiset

import mpst.utilities.Multiset

/* IDEA:

  @ telmo -
*/

object NetworkMultisetWrapper extends SOS[Action,NetStateWrapper]:
  override def accepting(state:NetStateWrapper):Boolean =
    val (locals,_,_) = state
    NetworkMultiset.accepting(locals)
  end accepting

  override def next[A>:Action](state:NetStateWrapper):Set[(A,NetStateWrapper)] =
    val (locals,pending,localEnv) = state
    for (nextAction,nextLocals,nextPending) <- NetworkMultiset.next(locals,pending)(using localEnv) yield
      val nextState = (nextLocals,nextPending,localEnv)
      nextAction -> nextState
  end next
end NetworkMultisetWrapper