package mpst.operational_semantic

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.Multiset

/* IDEA:
  - MS & CS Networks
  - CS Net took from choreo
  - problem: environment is applied to global (only)

  unrelated:
*/

object Network:
  object NetworkMultiset:
    // @ telmo - check with prof. José Proença
    def accepting(locals:Set[(Participant,Local)]):Boolean =
      for local <- locals yield
        if !MPSTSemantic.accepting(local._2) then return false
      true
    end accepting

    def next[A>:Action](locals:Set[(Participant,Local)], pending:Multiset[Action])(using environment:Map[Participant,Map[Variable,Local]]):Set[(A,Set[(Participant,Local)],Multiset[Action])] =
      nextAuxiliary(locals,pending)(using environment)
    end next

    private def nextAuxiliary[A>:Action](locals:Set[(Participant,Local)], pending:Multiset[Action])(using environment:Map[Participant,Map[Variable,Local]]):Set[(A,Set[(Participant,Local)],Multiset[Action])] =
      val nextNetwork = for local <- locals yield
        val nextEntry = getNextEntry(local,pending)
        for (nextAction,nextLocal,nextPending) <- nextEntry yield
          val nextLocals = locals-local+nextLocal
          (nextAction,nextLocals,nextPending)
      nextNetwork.flatten
    end nextAuxiliary

    private def getNextEntry(local:(Participant,Local), pending:Multiset[Action])(using environment:Map[Participant,Map[Variable,Local]]):Set[(Action,(Participant,Local),Multiset[Action])] =
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using environment(local._1)) if notBlocked(nextAction,pending) yield
        val nextPending = getNextPending(nextAction,pending)
        (nextAction,local._1 -> nextLocal,nextPending)
    end getNextEntry

    private def getNextPending(action:Action,pending:Multiset[Action]):Multiset[Action] =
      action match
        case Send   (agentA,agentB,message,sort) => pending + Send(agentA,agentB,message,sort)
        case Receive(agentA,agentB,message,sort) => pending - Send(agentB,agentA,message,sort)
        case protocol => throw new RuntimeException(s"unexpected protocol found in [$protocol]\n")
    end getNextPending

    private def notBlocked(action:Action,pending:Multiset[Action]):Boolean =
      action match
        case Send   (_, _, _, _) => true
        case Receive(agentA,agentB,message,sort) => pending `contains` Send(agentB, agentA, message, sort)
        case protocol => throw new RuntimeException(s"unexpected protocol found in [$protocol]\n")
    end notBlocked
  end NetworkMultiset
  /*
    TACKLE CAUSAL
  */
end Network