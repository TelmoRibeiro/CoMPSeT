package mpst.operational_semantic

import mpst.syntax.Protocol.{Action, Label, Local, Participant, Recv, Send, matchingAction}
import mpst.utility.Environment.Environment
import mpst.utility.Multiset

import scala.collection.immutable.Queue


object Network:
  object NetworkCausal:
    type ChannelQueue = Map[(Participant, Participant), Queue[Label]]

    def accepting(locals: Set[(Participant, Local)], pending: ChannelQueue): Boolean =
      locals.forall{ case _ -> local => MPSTSemantic.accepting(local) } && pending.isEmpty
    end accepting

    private type CausalLocalsReductions = Set[(Action, Set[(Participant, Local)], ChannelQueue)]

    def next[A >: Action](locals: Set[(Participant, Local)], pending: ChannelQueue)(using environment: Environment): CausalLocalsReductions =
      locals.flatMap(local =>
        localReduction(local, pending).map {
          case (nextAction, nextLocal, nextPending) =>
            (nextAction, locals - local + nextLocal, nextPending)
        }
      )
    end next

    private type CausalLocalReductions = Set[(Action, (Participant, Local), ChannelQueue)]

    private def localReduction(local: (Participant, Local), pending: ChannelQueue)(using environment: Environment): CausalLocalReductions =
      def notBlocked(action: Action, pending: ChannelQueue): Boolean = action match
        case _: Send => true
        case recvAction: Recv =>
          pending.getOrElse(recvAction.sender -> recvAction.receiver, Queue.empty).headOption.contains(recvAction.label)
      end notBlocked

      def nextPending(action: Action, pending: ChannelQueue): ChannelQueue = action match
        case sendAction: Send =>
          val updatedQueue = pending.getOrElse(sendAction.sender -> sendAction.receiver, Queue.empty).enqueue(sendAction.label)
          pending + (sendAction.sender -> sendAction.receiver -> updatedQueue)
        case recvAction: Recv =>
          val key = recvAction.sender -> recvAction.receiver
          val updatedQueue = pending.getOrElse(key, Queue.empty).dequeue._2
          if updatedQueue.isEmpty then pending - key else pending + (key -> updatedQueue)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using environment(local._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, local._1 -> nextLocal, nextPending(nextAction, pending))
    end localReduction
  end NetworkCausal


  object NetworkNonCausal:
    def accepting(locals: Set[(Participant, Local)], pending: Multiset[Action]): Boolean =
      locals.forall{ case _ -> local => MPSTSemantic.accepting(local) } && pending.isEmpty
    end accepting

    private type NonCausalLocalsReductions = Set[(Action, Set[(Participant, Local)], Multiset[Action])]

    def next[A >: Action](locals: Set[(Participant, Local)], pending: Multiset[Action])(using environment: Environment): NonCausalLocalsReductions =
      locals.flatMap (
        local => nextReduction(local, pending).map {
          case (nextAction, nextLocal, nextPending) =>
            (nextAction, locals - local + nextLocal, nextPending)
        }
      )
    end next

    private type NonCausalLocalReductions = Set[(Action, (Participant, Local), Multiset[Action])]

    private def nextReduction(local: (Participant, Local), pending: Multiset[Action])(using environment: Environment): NonCausalLocalReductions =
      def notBlocked(action: Action, pending: Multiset[Action]): Boolean = action match
        case _: Send => true
        case recvAction: Recv => pending contains matchingAction(recvAction)
      end notBlocked

      def nextPending(action: Action, pending: Multiset[Action]): Multiset[Action] = action match
        case sendAction: Send => pending + sendAction
        case recvAction: Recv => pending - matchingAction(recvAction)
      end nextPending

      for nextAction -> nextLocal <- MPSTSemantic.next(local._2)(using environment(local._1))
        if notBlocked(nextAction, pending)
      yield (nextAction, local._1 -> nextLocal, nextPending(nextAction, pending))
    end nextReduction
  end NetworkNonCausal
end Network