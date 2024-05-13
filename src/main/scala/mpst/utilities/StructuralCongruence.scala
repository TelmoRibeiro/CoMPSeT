package mpst.utilities

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import scala.annotation.tailrec

/* IDEA:
  - attempt to make the now @deprecated "Simplifier" concise
  - only structural congruence

  problem:
    yet to be tested
    Sequence(End,_) should be a wellformedness requirement
    NonGuarded      should be a wellformedness requirement
    check well formedness for recursion ( terminal case + recursive case )
    not related: xor and & can be derived by ! and ?
*/

object StructuralCongruence:
  private def runOnce(protocol:Protocol): Protocol =
    protocol match
      // terminal cases //
      case Interaction(_,_,_,_) => protocol
      case Send   (_,_,_,_) => protocol
      case Receive(_,_,_,_) => protocol
      case RecursionCall(_) => protocol
      case Skip             => protocol
      // recursive: associate ";", "||" and "+" //
      case Sequence(Sequence(protocolA,protocolB),protocolC) => runOnce(Sequence(protocolA,Sequence(protocolB,protocolC)))
      case Parallel(Parallel(protocolA,protocolB),protocolC) => runOnce(Parallel(protocolA,Parallel(protocolB,protocolC)))
      case Choice  (Choice  (protocolA,protocolB),protocolC) => runOnce(Choice  (protocolA,Choice  (protocolB,protocolC)))
      // recursive: skip //
      case Sequence(protocolA,Skip) => runOnce(protocolA) // @ telmo - can I count this as Struct.Congruence?
      case Sequence(Skip,protocolB) => runOnce(protocolB) // @ telmo - can I count this as Struct.Congruence?
      case Parallel(protocolA,Skip) => runOnce(protocolA) // @ telmo - usually literature allows this on "0"
      case Parallel(Skip,protocolB) => runOnce(protocolB) // @ telmo - usually literature allows this on "0"
      // @ telmo - CHOICE IS DANGEROUS!
      case RecursionFixedPoint(_,Skip) => Skip            // @ telmo - usually literature allows this on "0"
      /* @ telmo -
        choreo allows
        Choice(c1,c1) => runOnce(c1)
        does this count as structural congruence?
      */
      // recursive: default //
      case Sequence(protocolA,protocolB) => Sequence(runOnce(protocolA),runOnce(protocolB))
      case Parallel(protocolA,protocolB) => Parallel(runOnce(protocolA),runOnce(protocolB))
      case Choice  (protocolA,protocolB) => Choice  (runOnce(protocolA),runOnce(protocolB))
      case RecursionFixedPoint(variable,protocolB) => RecursionFixedPoint(variable,runOnce(protocolB))
  end runOnce

  @tailrec
  private def run(protocol:Protocol):Protocol =
    val congruentProtocol = runOnce(protocol)
    if  congruentProtocol == protocol
    then protocol
    else run(congruentProtocol)
  end run

  def apply(protocol:Protocol):Protocol =
    run(protocol)
  end apply
end StructuralCongruence