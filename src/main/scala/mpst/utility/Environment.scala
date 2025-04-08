package mpst.utility

import mpst.projection.StandardProjection
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

object Environment:
  type SingleEnvironment = Map[Variable, Protocol]
  type Environment       = Map[Participant, SingleEnvironment]

  def globalEnvironment(global: Global): SingleEnvironment =
    if !isGlobal(global) then throw RuntimeException(s"unexpected local type found in [$global]\n")
    protocolEnvironment(global)(using Map.empty)
  end globalEnvironment

  def localsEnvironment(global: Global): Environment =
    if !isGlobal(global) then throw RuntimeException(s"unexpected local type found in [$global]\n")
    StandardProjection.projectionWithParticipant(global).map {
      case participant -> local => participant -> protocolEnvironment(local)(using Map.empty)
    }.toMap
  end localsEnvironment

  private def protocolEnvironment(protocol: Protocol)(using environment: SingleEnvironment): SingleEnvironment = protocol match
    case _: Interaction | _: Action | _: RecursionCall | Skip => environment
    case Sequence(protocolA, protocolB) =>
      protocolEnvironment(protocolB)(using protocolEnvironment(protocolA))
    case Parallel(protocolA, protocolB) =>
      protocolEnvironment(protocolA) ++ protocolEnvironment(protocolB)
    case Choice  (protocolA, protocolB) =>
      protocolEnvironment(protocolA) ++ protocolEnvironment(protocolB)
    case RecursionFixedPoint(variable, protocolB) =>
      protocolEnvironment(protocolB)(using environment + (variable -> protocol))
    case RecursionKleeneStar(protocolA) =>
      protocolEnvironment(protocolA)
  end protocolEnvironment
end Environment