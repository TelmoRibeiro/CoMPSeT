package mpst.frontend

import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Setting.{allActiveFrom, allActiveLeavesFrom}
import caos.frontend.Site.{getSetting, setSetting}
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS.toMermaid
import caos.view.{Code, Mermaid}

import mpst.frontend.MessageSequenceChart.*
import mpst.frontend.caos_wrapper.MPSTEnvironmentWrapper.MPSTSemanticWrapper
import mpst.frontend.caos_wrapper.NetworkWrapper.{NetworkCausal, NetworkMultiset}
import mpst.frontend.caos_wrapper.SyncEnvironmentWrapper.SyncTraverseWrapper
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{PlainMergeProjection, FullMergeProjection}
import mpst.syntax.Protocol.{Recv, Action, Global, Local, Participant, hasFixedPointRecursion, hasKleeneStarRecursion, hasParallel, toString}
import mpst.utility.Environment.{Environment, SingleEnvironment, localsEnvironment}
import mpst.utility.Multiset
import mpst.wellformedness.{WellBounded, WellBranched, WellChanneled}


case class Widgets(prefix: String):
  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map { case k -> v => s"$k -> $v" }.mkString("\n")

  private def localsWithParticipant(enabledMerge: Set[Setting])(using global: Global): Set[(Participant, Local)] =
    val localsWithParticipantOption = enabledMerge match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some(PlainMergeProjection.projectionWithParticipant(global))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some(FullMergeProjection.projectionWithParticipant(global))
      case _ => None
    val localsWithParticipant = localsWithParticipantOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    allChecksLocals(localsWithParticipant)
    localsWithParticipant
  end localsWithParticipant

  private def checkLocals(localsWithParticipant: Set[(Participant, Local)], localCondition: Local => Boolean, settingCondition: => Boolean, prefix: String): Unit =
    localsWithParticipant.foreach {
      case participant -> local if localCondition(local) && settingCondition =>
        throw RuntimeException(s"$prefix - present on participant [$participant]")
      case _ =>
    }
  end checkLocals

  private def allChecksLocals(localsWithParticipant: Set[(Participant, Local)]): Unit =
    checkLocals(localsWithParticipant, hasParallel, !getSetting.allActiveLeavesFrom("Semantics").exists(_.name == "Parallel"), "Parallel")
    checkLocals(localsWithParticipant, hasKleeneStarRecursion, !getSetting.allActiveLeavesFrom("Semantics.Recursion").exists(_.name == "Kleene Star"), "Recursion Kleene Star")
    checkLocals(localsWithParticipant, hasFixedPointRecursion, !getSetting.allActiveLeavesFrom("Semantics.Recursion").exists(_.name == "Fixed Point"), "Recursion Fixed Point")
  end allChecksLocals

  val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Message Sequence Chart"
      -> view(MessageSequenceChart.apply, Mermaid),

    "Global"
      -> view((global: Global) => s"${global.toString}", Code("java")),

    "Locals"
      -> view((global: Global) =>
      localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global).map { case participant -> local => s"$participant -> $local" }.mkString("\n"),
      Code("java")
    ).setRender(getSetting.allActiveFrom("Semantics").exists(_.name == "Merge")),

    "Local Automata"
      -> viewMerms((global: Global) =>
      val environment = localsEnvironment(global)
      localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global).map { case participant -> local =>
        val lts = caos.sos.SOS.toMermaid(
          MPSTSemanticWrapper,
          local -> environment(participant),
          (local: Local, environment: SingleEnvironment) =>
            environment.toPrettyPrint,
          _.toString,
          100,
        )
        participant -> lts
      }.toList
    ).setRender(getSetting.allActiveFrom("Semantics").exists(_.name == "Merge")),


    "Local Compositional Automata - Synchronous"
      -> lts((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
      allChecksLocals(initialState._1)
      initialState,
      SyncTraverseWrapper,
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Sync")),

    "Local Compositional Automata - Asynchronous (Causal)"
      -> lts((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
      allChecksLocals(initialState._1)
      initialState,
      NetworkCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Causal)")),

    "Local Compositional Automata - Asynchronous (Non-Causal)"
      -> lts((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
      allChecksLocals(initialState._1)
      initialState,
      NetworkMultiset,
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Step-by-Step - Synchronous"
      -> steps((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
      allChecksLocals(initialState._1)
      initialState,
      SyncTraverseWrapper,
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Sync")),

    "Step-by-Step Asynchronous (Causal)"
      -> steps((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
      allChecksLocals(initialState._1)
      initialState,
      NetworkCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Causal)")),

    "Step-by-Step Asynchronous (Non-Causal)"
      -> steps((global: Global) =>
      val initialStateOption = getSetting.allActiveLeavesFrom("Semantics.Merge") match
        case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
          Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
        case enabledMerge if enabledMerge.exists(_.name == "Full") =>
          Some((FullMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
        case _ => None
      val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
      allChecksLocals(initialState._1)
      initialState,
      NetworkMultiset,
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Sync vs Async (Causal) - Bisimulation"
      -> compareBranchBisim(
      SyncTraverseWrapper,
      NetworkCausal,
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), None, localsEnvironment(global)),
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), Map.empty, localsEnvironment(global)),
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Causal)")),

    "Sync vs Async (Non-Causal) - Bisimulation"
      -> compareBranchBisim(
      SyncTraverseWrapper,
      NetworkMultiset,
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), None, localsEnvironment(global)),
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), Multiset(), localsEnvironment(global)),
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Async (Causal) vs Async (Non-Causal) - Bisimulation"
      -> compareBranchBisim(
      NetworkCausal,
      NetworkMultiset,
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), Map.empty, localsEnvironment(global)),
      (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global), Multiset(), localsEnvironment(global)),
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Causal)") && getSetting.allActiveLeavesFrom("Semantics.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Well Channeled"
      -> check((global: Global) =>
      if !WellChanneled(global) then Seq(s"[$global] is not well channeled") else Seq.empty
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Extra Requirements").exists(_.name == "Well Channeled")),

    "Well Branched"
      -> check((global: Global) =>
      if !WellBranched(global) then Seq(s"[$global] is not well branched") else Seq.empty
    ).setRender(getSetting.allActiveLeavesFrom("Semantics.Extra Requirements").exists(_.name == "Well Branched")),

    "Well Bounded"
      -> check((global: Global) =>
      if !WellBounded(global) then Seq(s"[$global] is not well bounded") else Seq.empty
    ),
  )
end Widgets