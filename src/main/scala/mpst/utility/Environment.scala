package mpst.utility

import mpst.projection.AsyncProjection
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

/* @ telmo
  IDEA:
    => [[Environment]] represents the natural mappings between recursion [[Variable]] and the protocol they unfold into
  ISSUES:
    => we assume no variable repetition
      I can try to relax this by reviewing RenameRecursion
  REVIEWED:
    => AFFIRMATIVE
*/

object Environment:
  type SingleEnvironment = Map[Variable, Protocol]
  type Environment       = Map[Participant, SingleEnvironment]

  def globalEnvironment(global: Global): SingleEnvironment =
    if !isGlobal(global) then throw RuntimeException(s"unexpected local type found in [$global]\n")
    protocolEnvironment(global)(using Map.empty)
  end globalEnvironment

  def localsEnvironment(global: Global): Environment =
    if !isGlobal(global) then throw RuntimeException(s"unexpected local type found in [$global]\n")
    AsyncProjection.projectionWithAgent(global).map {
      case participant -> local => participant -> protocolEnvironment(local)(using Map.empty)
    }.toMap
  end localsEnvironment

  private def protocolEnvironment(protocol: Protocol)(using environment: SingleEnvironment): SingleEnvironment = protocol match
    case Interaction(_, _, _, _) => environment
    case Send   (_, _, _, _) => environment
    case Receive(_, _, _, _) => environment
    case RecursionCall(_) => environment
    case Skip => environment
    case Sequence(protocolA, protocolB) =>
      protocolEnvironment(protocolB)(using protocolEnvironment(protocolA))
    case Parallel(protocolA, protocolB) =>
      protocolEnvironment(protocolA) ++ protocolEnvironment(protocolB)
    case Choice  (protocolA, protocolB) =>
      protocolEnvironment(protocolA) ++ protocolEnvironment(protocolB)
    case RecursionFixedPoint(variable, protocolB) =>
      protocolEnvironment(protocolB)(using environment + (variable -> protocolB))
  end protocolEnvironment
end Environment