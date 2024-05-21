/*
package mpst.operational_semantic

import mpst.operational_semantic.Network.NetworkMultiset
import mpst.syntax.Type.*
import mpst.utilities.Multiset

type LocalEnv     = Map[Agent,Map[Variable,Local]]
type StateWrapper = ((Action,Set[(Agent, Local)],Multiset[Action]),LocalEnv)

object NetworkMultisetWrapper:
  def next[A>:Action](state:StateWrapper):Set[(A,StateWrapper)] =
    val (action,locals,pending,localEnv) = state
    for (nextAction,nextLocals,nextPending) <- NetworkMultiset.nextNetwork(locals,pending)(using localEnv) yield
      nextAction -> (nextAction,nextLocals,nextPending,localEnv)
  end next
end NetworkMultisetWrapper
*/