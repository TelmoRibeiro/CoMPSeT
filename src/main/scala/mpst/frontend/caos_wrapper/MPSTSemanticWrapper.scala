package mpst.frontend.caos_wrapper

import mpst.operational_semantic.MPSTSemantic
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

import caos.sos.SOS

/* IDEA:

  @ telmo -
*/

private type StateWrapper = (Protocol, Map[Variable, Protocol])

object MPSTSemanticWrapper extends SOS[Action,StateWrapper]:
  override def accepting(state:StateWrapper):Boolean =
    val protocol -> _ = state
    MPSTSemantic.accepting(protocol)
  end accepting

  override def next[A>:Action](state:StateWrapper):Set[(A,StateWrapper)] =
    val protocol -> environment = state
    for nextAction -> nextProtocol <- MPSTSemantic.next(protocol)(using environment) yield
      nextAction -> (nextProtocol -> environment)
  end next
end MPSTSemanticWrapper