package mpst.projection

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.StructuralCongruence

/* IDEA:
  - allow the projection of *global types* into *local types*
  - this new version was not yet tested

  problem: map to papers
*/

object AsyncProjection:
  def projectionWithAgent(global:Global):Set[(Agent,Local)] =
    val agents = getAgents(global)
    for agent <- agents yield
      val maybeLocal = getProjection(global)(using agent)
      maybeLocal match
        case Some(local) => agent -> StructuralCongruence(local)
        case None        => throw new RuntimeException(s"projection undefined for agent [$agent] in [$global]\n")
  end projectionWithAgent

  def projection(global:Global):Set[Local] =
    val agents = getAgents(global)
    for agent <- agents yield
      val maybeLocal = getProjection(global)(using agent)
      maybeLocal match
        case Some(local) => StructuralCongruence(local)
        case None        => throw new RuntimeException(s"projection undefined for agent [$agent] in [$global]\n")
  end projection

  private def getProjection(global:Protocol)(using agent:Agent):Option[Protocol] =
    global match
      case Interaction(agentA,agentB,message,sort) =>
        if agent == agentA && agent != agentB then
          return Some(Send   (agentA,agentB,message,sort))
        if agent != agentA && agent == agentB then
          return Some(Receive(agentB,agentA,message,sort))
        if agent != agentA && agent != agentB then
          return Some(Skip)
        None
      case RecursionCall(_) =>
        Some(global)
      case Skip =>
        Some(global)
      case Sequence(globalA,globalB) =>
        val maybeLocalA = getProjection(globalA)
        val maybeLocalB = getProjection(globalB)
         maybeLocalA -> maybeLocalB match
          case Some(localA) -> Some(localB) => Some(Sequence(localA,localB))
          case _ -> _                       => None
      case Parallel(globalA,globalB) =>
        val maybeLocalA = getProjection(globalA)
        val maybeLocalB = getProjection(globalB)
        maybeLocalA -> maybeLocalB match
          case Some(localA) -> Some(localB) => Some(Parallel(localA,localB))
          case _ -> _                       => None
      case Choice(globalA,globalB) =>
        val maybeLocalA = getProjection(globalA)
        val maybeLocalB = getProjection(globalB)
        maybeLocalA -> maybeLocalB match
          case Some(localA) -> Some(localB) => Some(Choice(localA,localB))
          case _ -> _                       => None
      case RecursionFixedPoint(variable,globalB) =>
        val maybeLocalB = getProjection(globalB)
        maybeLocalB match
          // case Some(RecursionCall(variable)) => None // @ telmo - Gentle Approach ...
          case Some(localB) => Some(RecursionFixedPoint(variable,localB))
          case _            => None
      case _ => None
  end getProjection
end AsyncProjection