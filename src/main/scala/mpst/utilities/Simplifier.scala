package mpst.utilities

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import scala.annotation.tailrec

/* IDEA:
  - concepts initial from Choreo + lots of papers
  - structural congruence + "simplifying"

  @ telmo -
    problematic...
*/

@deprecated
object Simplifier:
  // check thoroughly for unforeseen consequences //
  private def simplifyOnce(protocol: Protocol): Protocol =
    protocol match
      // terminal cases //
      case Interaction(agentA,agentB,message,sort) => Interaction(agentA,agentB,message,sort)
      case Send   (agentA,agentB,message,sort) => Send   (agentA,agentB,message,sort)
      case Receive(agentA,agentB,message,sort) => Receive(agentA,agentB,message,sort)
      case RecursionCall(variable) => RecursionCall(variable)
      case Skip => Skip
      // associate ";", "||" and "+" //
      case Sequence(Sequence(protocolA,protocolB),protocolC) => simplifyOnce(Sequence(protocolA,Sequence(protocolB,protocolC)))
      case Parallel(Parallel(protocolA,protocolB),protocolC) => simplifyOnce(Parallel(protocolA,Parallel(protocolB,protocolC)))
      case Choice  (Choice  (protocolA,protocolB),protocolC) => simplifyOnce(Choice  (protocolA,Choice  (protocolB,protocolC)))
      // recursive "End"
      case Sequence(Skip,protocolB) => simplifyOnce(protocolB)
      case Parallel(Skip,protocolB) => simplifyOnce(protocolB)
      case Parallel(protocolA,Skip) => simplifyOnce(protocolA)
      case RecursionFixedPoint(_,Skip) => Skip
      // recursive cases //
      case Sequence(protocolA,protocolB) => Sequence(simplifyOnce(protocolA),simplifyOnce(protocolB))
      case Parallel(protocolA,protocolB) => Parallel(simplifyOnce(protocolA),simplifyOnce(protocolB))
      case Choice  (protocolA,protocolB) if protocolA == protocolB => simplifyOnce(protocolA)
      case Choice  (protocolA,protocolB) => Choice(simplifyOnce(protocolA),simplifyOnce(protocolB))
      case RecursionFixedPoint(variableA, RecursionCall(variableB)) if variableA == variableB => Skip // non-guarded recursion //
      case RecursionFixedPoint(variable,protocolB) => RecursionFixedPoint(variable,simplifyOnce(protocolB))
  end simplifyOnce

  @tailrec
  private def simplify(protocol:Protocol):Protocol =
    val simplifiedProtocol = simplifyOnce(protocol)
    if  simplifiedProtocol == protocol then protocol else simplify(simplifiedProtocol)
  end simplify

  def apply(protocol:Protocol):Protocol =
    simplify(protocol)
  end apply
end Simplifier