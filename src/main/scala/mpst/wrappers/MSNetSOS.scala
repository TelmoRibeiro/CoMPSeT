/*
package mpst.wrappers

import caos.sos.SOS
import mpst.syntax.Type.*

/*
  IDEA:
    integration between MPST and CAOS
*/

import mpst.operational_semantic.Network.NetworkMultiset
import mpst.operational_semantic.MPSTSemantic
import mpst.projection.AsyncProjection
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.utilities.Multiset

type NetAction = Action
type NetState  = (Action,Set[(Agent,Local)],Multiset[Action])

object MSNetSOS extends caos.sos.SOS[Action,(Action,Set[(Agent,Local)],Multiset[Action])]:
  override def accepting(state:NetState):Boolean =
    val (action,locals,pending) = state
    for local <- locals yield
      if !MPSTSemantic.accept(local._2) then return false
    true
  end accepting

  override def next(state:NetState)(using environment:Map[Agent,Map[Variable,Local]]):Set[(NetAction,NetState)] =
    val (action,locals,pending) = state
    for (nextAction,nextLocals,nextPending) <-  NetworkMultiset.nextNetwork(locals,pending)(using environment) yield
      nextAction -> (nextAction,locals,nextPending)
  end next
end MSNetSOS
*/