package mpst.utilities

import mpst.projection.*
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

/*
  IDEA:
  - get the environment (MAP from RecursionVariable to GlobalType)
  - assumes whole initial type
  - assumes no repetition of recursion variables in different scopes
    - this assumption can be remove if I complete RenameRecursion
*/

object Environment:
  def globalEnv(global:Global):Map[Variable,Global] =
    if !isGlobal(global) then throw new RuntimeException(s"unexpected local type found in [$global]\n")
    protocolEnv(global)(using Map())
  end globalEnv

  def localEnv(global:Global):Map[Agent,Map[Variable,Local]] =
    if !isGlobal(global) then throw new RuntimeException(s"unexpected local type found in [$global]\n")
    val localSet = for agent -> local <- AsyncProjection.projectionWithAgent(global) yield
      agent -> protocolEnv(local)(using Map())
    localSet.toMap.map{case agent -> environment => agent -> environment}
  end localEnv

  private def protocolEnv(protocol:Protocol)(using environment:Map[Variable,Protocol]):Map[Variable,Protocol] =
    protocol match
      case Interaction(_,_,_,_) => environment
      case Send   (_,_,_,_) => environment
      case Receive(_,_,_,_) => environment
      case RecursionCall(_) => environment
      case Skip             => environment
      case Sequence(protocolA,protocolB) =>
        val environmentA = protocolEnv(protocolA)
        val environmentB = protocolEnv(protocolB)(using environmentA)
        environmentB
      case Parallel(protocolA,protocolB) => protocolEnv(protocolA) ++ protocolEnv(protocolB)
      case Choice  (protocolA,protocolB) => protocolEnv(protocolA) ++ protocolEnv(protocolB)
      case RecursionFixedPoint(variable,protocolB) => protocolEnv(protocolB)(using environment+(variable->protocol))
  end protocolEnv
end Environment