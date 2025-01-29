package mpst.operational_semantic

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.utility.StructuralCongruence

import caos.sos.SOS
import caos.sos.SOS._

/* IDEA:
  - attempt at a strong sequencing semantic - original idea stated in Choreo

  @ telmo -
    RecursionCall is quite hacky!
*/

object MPSTSemantic:
  def accepting(protocol: Protocol):Boolean =
    MPSTSemantic.acceptAuxiliary(protocol)
  end accepting

  def next[A >: Action](protocol: Protocol)(using environment: Map[Variable, Protocol]): Set[(A, Protocol)] =
    MPSTSemantic.nextAuxiliary(protocol)(using environment).toSet
  end next

  private def acceptAuxiliary(protocol:Protocol):Boolean = protocol match
    case _: Interaction | _: Send | _: Receive | _: RecursionCall => false
    case Skip => true
    case Sequence(protocolA, protocolB) => acceptAuxiliary(protocolA) && acceptAuxiliary(protocolB)
    case Parallel(protocolA, protocolB) => acceptAuxiliary(protocolA) && acceptAuxiliary(protocolB)
    case Choice  (protocolA, protocolB) => acceptAuxiliary(protocolA) || acceptAuxiliary(protocolB)
    case RecursionFixedPoint(_, protocolB) => acceptAuxiliary(protocolB)
    case _: RecursionKleeneStar => true // @ telmo - not checked!
  end acceptAuxiliary

  private def nextAuxiliary(protocol: Protocol)(using environment: Map[Variable, Protocol]): List[(Action, Protocol)] = protocol match
    case Interaction(participantA, participantB, label, sort) => List(Send(participantA, participantB, label, sort) -> Receive(participantB, participantA, label, sort))
    case _: Send | _: Receive    => List(protocol -> Skip)
    case RecursionCall(variable) => nextAuxiliary(environment(variable)) // @ telmo - check RecursionCall(_) for parallel and choice
    case Skip => Nil
    case Sequence(protocolA, protocolB) =>
      val nextA = nextAuxiliary(protocolA)
      // val nextB = nextAuxiliary(protocolB)
      val resultA = for nextActionA -> nextProtocolA <- nextA yield
        nextActionA -> StructuralCongruence(Sequence(nextProtocolA, protocolB))
      val resultB = if accepting(protocolA) then nextAuxiliary(protocolB) else Nil // @ telmo - introducing error here
      resultA ++ resultB
    case Parallel(protocolA, protocolB) =>
      val resultA = for nextActionA -> nextProtocolA <- nextAuxiliary(protocolA) yield
        nextActionA -> StructuralCongruence(Parallel(nextProtocolA, protocolB))
      val resultB = for nextActionB -> nextProtocolB <- nextAuxiliary(protocolB) yield
        nextActionB -> StructuralCongruence(Parallel(protocolA, nextProtocolB))
      resultA ++ resultB
    case Choice(protocolA,protocolB) =>
      nextAuxiliary(protocolA) ++ nextAuxiliary(protocolB)
    case RecursionFixedPoint(_, protocolB) => protocolB match
      case _: RecursionCall => throw RuntimeException(s"[$protocol] is not guarded")
      case _ => nextAuxiliary(protocolB)
    case RecursionKleeneStar(protocolA) =>
      for nextActionA -> nextProtocolA <- nextAuxiliary(protocolA) yield
        nextActionA -> Sequence(nextProtocolA, protocol)
  end nextAuxiliary

  private def unfoldRecursion(protocol: Protocol)(using environment: Map[Variable, Protocol]): Protocol = protocol match
    case RecursionCall(variable) => environment(variable)
    case _ => protocol
  end unfoldRecursion

  private def recursionFree(recursionVariable: Variable, protocol: Protocol): Protocol =
    def recursionFreeAuxiliary(protocol: Protocol)(using recursionVariable: Variable): Protocol = protocol match
      case _: Interaction | _: Send | _: Receive | Skip => protocol
      case RecursionCall(variable) => if variable == recursionVariable then Skip else protocol
      case Sequence(protocolA, protocolB) => Sequence(recursionFreeAuxiliary(protocolA), recursionFreeAuxiliary(protocolB))
      case Parallel(protocolA, protocolB) => Parallel(recursionFreeAuxiliary(protocolA), recursionFreeAuxiliary(protocolB))
      case Choice  (protocolA, protocolB) => Choice  (recursionFreeAuxiliary(protocolA), recursionFreeAuxiliary(protocolB))
      case RecursionFixedPoint(variable, protocolB) => RecursionFixedPoint(variable, recursionFreeAuxiliary(protocolB))
      case RecursionKleeneStar(protocolA) => RecursionKleeneStar(recursionFreeAuxiliary(protocolA))
    end recursionFreeAuxiliary

    StructuralCongruence(recursionFreeAuxiliary(protocol)(using recursionVariable))
  end recursionFree
end MPSTSemantic