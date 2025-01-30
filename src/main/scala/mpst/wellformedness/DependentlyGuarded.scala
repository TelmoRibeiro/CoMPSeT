package mpst.wellformedness

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

/* IDEA:
  - check well-formedness on *recursion*
    - a loop is dependently guarded if:
      - for all actions l
      - the loop body can only *partially terminate* for l
      - if l does not occur in the loop body at all
    - other words:
      - if a participant occurs in some branch of the loop
      - it must occur in all branches of the loop

  problem: not yet tested
*/

object DependentlyGuarded:
  private def dependentlyGuarded(global: Global): Boolean = global match
      case _: Interaction | _: RecursionCall | Skip => true
      case Sequence(globalA, globalB) => dependentlyGuarded(globalA) && dependentlyGuarded(globalB)
      case Parallel(globalA, globalB) => dependentlyGuarded(globalA) && dependentlyGuarded(globalB)
      case Choice  (globalA, globalB) => dependentlyGuarded(globalA) && dependentlyGuarded(globalB)
      case RecursionFixedPoint(_, globalB) =>
        for participant <- getParticipants(globalB) yield dependentlyGuardedAuxiliary(globalB, participant) match
          case Some(global) if globalB != global => throw new RuntimeException(s"[$globalB] partially evaluates to [$global] for participant [$participant] yet they are different")
          case _ =>
        dependentlyGuarded(globalB)
      case RecursionKleeneStar(globalA) =>
        for participant <- getParticipants(globalA) yield dependentlyGuardedAuxiliary(globalA, participant) match
          case Some(global) if globalA != global => throw new RuntimeException(s"[$globalA] partially evaluates to [$global] for participant [$participant] yet they are different")
          case _ =>
        dependentlyGuarded(globalA)
      case _ => throw new RuntimeException(s"found unexpected [$global]")
  end dependentlyGuarded

  private def dependentlyGuardedAuxiliary(global: Global, participant: Participant): Option[Global] = global match
    case Interaction(sender, receiver, _, _) =>
      if participant == sender || participant == receiver then None else Some(global)
    case _: RecursionCall | Skip => Some(global)
    case Sequence(globalA, globalB) => dependentlyGuardedAuxiliary(globalA, participant) -> dependentlyGuardedAuxiliary(globalB, participant) match
      case Some(gA) -> Some(gB) => Some(Sequence(gA, gB))
      case _ -> _ => None
    case Parallel(globalA, globalB) => dependentlyGuardedAuxiliary(globalA, participant) -> dependentlyGuardedAuxiliary(globalB, participant) match
      case Some(gA) -> Some(gB) => Some(Parallel(gA, gB))
      case _ -> _ => None
    case Choice  (globalA, globalB) =>
      val maybeGlobalA = dependentlyGuardedAuxiliary(globalA, participant)
      val maybeGlobalB = dependentlyGuardedAuxiliary(globalB, participant)
      maybeGlobalA -> maybeGlobalB match
        case None -> maybeGlobalB => maybeGlobalB
        case maybeGlobalA -> None => maybeGlobalA
        case Some(gA) -> Some(gB) => Some(Choice(gA, gB))
    case RecursionFixedPoint(_, globalB) => dependentlyGuardedAuxiliary(globalB, participant) match
      case Some(gB) => Some(gB)
      case _ => Some(Skip)
    case RecursionKleeneStar(globalA)    => dependentlyGuardedAuxiliary(globalA, participant) match
      case Some(gb) => Some(gb)
      case _ => Some(Skip)
    case _ => throw new RuntimeException(s"found unexpected [$global]\n")
  end dependentlyGuardedAuxiliary

  def apply(global:Global):Boolean =
    dependentlyGuarded(global)
  end apply
end DependentlyGuarded