package mpst.operational_semantic

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.utility.Environment.SingleEnvironment
import mpst.utility.StructuralCongruence

import caos.sos.SOS
import caos.sos.SOS._


object MPSTSemantic:
  def accepting(protocol: Protocol): Boolean =
    MPSTSemantic.acceptAuxiliary(protocol)
  end accepting

  def next[A >: Action](protocol: Protocol)(using environment: SingleEnvironment): Set[(A, Protocol)] =
    MPSTSemantic.nextAuxiliary(protocol)(using environment).toSet
  end next

  private def acceptAuxiliary(protocol: Protocol): Boolean = protocol match
    case _: Interaction | _: Action | _: RecursionCall => false
    case Skip => true
    case Sequence(protocolA, protocolB) => acceptAuxiliary(protocolA) && acceptAuxiliary(protocolB)
    case Parallel(protocolA, protocolB) => acceptAuxiliary(protocolA) && acceptAuxiliary(protocolB)
    case Choice  (protocolA, protocolB) => acceptAuxiliary(protocolA) || acceptAuxiliary(protocolB)
    case RecursionFixedPoint(_, protocolB) => acceptAuxiliary(protocolB)
    case RecursionKleeneStar(_)            => true
  end acceptAuxiliary

  private def nextAuxiliary(protocol: Protocol)(using environment: SingleEnvironment): List[(Action, Local)] = protocol match
    case interaction: Interaction =>
      List(Send(interaction.sender, interaction.receiver, interaction.label) -> Recv(interaction.receiver, interaction.sender, interaction.label))
    case action: Action =>
      List(action -> Skip)
    case RecursionCall(variable) =>
      nextAuxiliary(environment(variable))
    case Skip =>
      Nil
    case Sequence(protocolA, protocolB) =>
      val nextA = nextAuxiliary(protocolA)
      val resultA = for nextActionA -> nextProtocolA <- nextA yield
        nextActionA -> StructuralCongruence(Sequence(nextProtocolA, protocolB))
      val resultB = if accepting(protocolA) then nextAuxiliary(protocolB) else Nil
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
        nextActionA -> StructuralCongruence(Sequence(nextProtocolA, protocol))
  end nextAuxiliary
end MPSTSemantic