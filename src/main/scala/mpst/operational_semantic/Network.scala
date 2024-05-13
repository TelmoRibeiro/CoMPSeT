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
    def nextNetwork(locals:Set[(Agent,Local)],pending:Multiset[Action])(using environment:Map[Agent,Map[Variable,Local]]):Set[(Action,Set[(Agent,Local)],Multiset[Action])] =
      val nextNetwork = for local <- locals yield
        val nextEntry = getNextEntry(local,pending)
        for (nextAction,nextLocal,nextPending) <- nextEntry yield
          val nextLocals = locals-local+nextLocal
          (nextAction,nextLocals,nextPending)
      nextNetwork.flatten
    end nextNetwork

    private def getNextEntry(local:(Agent,Local),pending:Multiset[Action])(using environment:Map[Agent,Map[Variable,Local]]):Set[(Action,(Agent,Local),Multiset[Action])] =
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

  object NetworkCausal:
    def nextNetwork(locals:Set[(Agent,Local)],pending:Queue)(using environment:Map[Agent,Map[Variable,Local]]):Set[(Action,Set[(Agent,Local)],Queue)] =
      val nextNetwork = for local <- locals yield
        val nextEntry = getNextEntry(local,pending)
        for (nextAction,nextLocal,nextPending) <- nextEntry yield
          val nextLocals = locals-local+nextLocal
          (nextAction,nextLocals,nextPending)
      nextNetwork.flatten
    end nextNetwork

    private def getNextEntry(local:(Agent,Local),pending:Queue)(using environment:Map[Agent,Map[Variable,Protocol]]):Set[(Action,(Agent,Local),Queue)] =
      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using environment(local._1)) if notBlocked(nextAction,pending) yield
        val nextPending = getNextPending(nextAction,pending)
        (nextAction,local._1 -> nextLocal,nextPending)
    end getNextEntry

    private def getNextPending(action:Action,pending:Queue):Queue =
      action match
        case Send(agentA,agentB,message,_) =>
          val entry = (agentA -> agentB) -> (pending.getOrElse(agentA -> agentB,Nil):::List(message))
          pending + entry
        case Receive(agentA,agentB,_,_) =>
          val entry = (agentB -> agentA) -> pending(agentB -> agentA).tail
          pending + entry
        case protocol => throw new RuntimeException(s"unexpected protocol found in [$protocol]\n")
    end getNextPending

    private def notBlocked(action:Action,network:Queue):Boolean =
      action match
        case Send(_,_,_,_) => true
        case Receive(agentA,agentB,message,sort) => (network contains agentB -> agentA) && network(agentB -> agentA).nonEmpty && network(agentB -> agentA).head == message
        case protocol => throw new RuntimeException(s"unexpected protocol found in [$protocol]\n")
    end notBlocked
  end NetworkCausal
end Network