package mpst.projection

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.StructuralCongruence

/* IDEA:
  - allow the projection of *global types* into *local types*
  - "Multiparty Synchronous Session Types"-ish projection

  problem:
    test with recursion
    ADD NON-GUARDED RECURSION RULE FROM GENTLE APPROACH...?
*/

object SyncProjection:
  def projectionWithAgent(global:Global):Set[(Agent,Local)] =
    val agents = getAgents(global)
    for agent <- agents yield
      val maybeLocal = getProjection(global)(using agent)
      maybeLocal match
        case Some(local) => agent -> StructuralCongruence(local)
        case None        => throw new RuntimeException(s"projection undefined for agent [$agent] in [$global]\n")
  end projectionWithAgent

  def projection(global:Protocol):Set[Protocol] =
    val agents = getAgents(global)
    for agent <- agents yield
      val maybeLocal = getProjection(global)(using agent)
      maybeLocal match
        case Some(local) => StructuralCongruence(local)
        case None        => throw new RuntimeException(s"projection undefined for agent [$agent] in [$global]\n")
  end projection

  private def getProjection(global:Global)(using agent:Agent):Option[Local] =
    global match
      // @ telmo - if conditions assert well-communication property
      // @ telmo - team automata
      // @ telmo - synchronous in branching pomsets
      // @ telmo - default & and xor
      case Interaction(agentA,agentB,message,sort) =>
        if agent == agentA && agent != agentB then
          return Some(Send   (agentA,agentB,message,sort))
        if agent != agentA && agent == agentB then
          return Some(Receive(agentB,agentA,message,sort))
        if agent != agentA && agent != agentB then
          return Some(Skip)
        throw new RuntimeException(s"projection undefined for agent [$agent] in [$global]\n")
      case RecursionCall(variable) =>
        Some(global)
      case Skip =>
        Some(global)
      case Sequence(globalA,globalB) =>
        val maybeLocalA = getProjection(globalA)
        val maybeLocalB = getProjection(globalB)
        maybeLocalA -> maybeLocalB match
          case Some(localA) -> Some(localB) => Some(Sequence(localA,localB))
          case _ -> _                       => throw new RuntimeException(s"projection undefined for [$agent] in [$global]\n")
      case Parallel(globalA,globalB) =>
        val agentsA = getAgents(globalA)
        val agentsB = getAgents(globalB)
        if (agentsA contains agent) && !(agentsB contains agent) then
          val maybeLocalA = getProjection(globalA)
          return maybeLocalA
        if !(agentsA contains agent) && (agentsB contains agent) then
          val maybeLocalB = getProjection(globalB)
          return maybeLocalB
        if !(agentsA contains agent) && !(agentsB contains agent) then
          return Some(Skip) // @ telmo - check this
        // problem in (G1, (G2,G3)) ~~ ((G1,G2),G3))
        // can p!q:x;G1 || p?q:x;G2 => G1||G2 but p!q:x;G1 || G0;q?p:x;G2
        throw new RuntimeException(s"projection undefined for [$agent] in [$global]\n")
      case Choice(globalA,globalB) =>
        val maybeLocalA = getProjection(globalA)
        val maybeLocalB = getProjection(globalB)
        maybeLocalA -> maybeLocalB match
          case Some(localA) -> Some(localB) => Some(Choice(localA,localB))
          case _ -> _                       => throw new RuntimeException(s"projection undefined for [$agent] in [$global]\n")
      case RecursionFixedPoint(variable,globalB) =>
        val agentsB = getAgents(globalB)
        if agentsB contains agent then
          val maybeLocalB = getProjection(globalB)
          maybeLocalB match
            case Some(localB) => return Some(RecursionFixedPoint(variable,localB))
            case _            => throw new RuntimeException(s"projection undefined for [$agent] in [$global]\n")
        Some(Skip) // @ telmo - check this
      case _ => None
  end getProjection
end SyncProjection