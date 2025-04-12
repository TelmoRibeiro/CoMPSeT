package mpst.frontend.auxiliary.wrappers

import caos.sos.SOS
import mpst.operational_semantic.MPSTSemantic
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*


object MPSTEnvironmentWrapper:
  private type StateWrapper = (Protocol, Map[Variable, Protocol])

  object MPSTSemanticWrapper extends SOS[Action, StateWrapper]:
    override def accepting(state: StateWrapper): Boolean = MPSTSemantic.accepting(state._1)

    override def next[A >: Action](state: StateWrapper): Set[(A, StateWrapper)] = state match
      case protocol -> environment => for nextAction -> nextProtocol <- MPSTSemantic.next(protocol)(using environment) yield
        nextAction -> (nextProtocol -> environment)
    end next
  end MPSTSemanticWrapper
end MPSTEnvironmentWrapper