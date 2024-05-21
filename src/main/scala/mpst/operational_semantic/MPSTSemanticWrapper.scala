package mpst.operational_semantic

import caos.sos.SOS
import mpst.syntax.Type.*
import mpst.syntax.Protocol.*
import mpst.operational_semantic.MPSTSemantic
import mpst.syntax.Protocol

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