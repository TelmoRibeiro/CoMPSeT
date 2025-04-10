package mpst.frontend.auxiliary

import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Site.getSetting
import caos.frontend.widgets.WidgetInfo
import caos.view.{Code, Mermaid}
import mpst.frontend.auxiliary.view.MessageSequenceChart
import mpst.frontend.auxiliary.wrappers.MPSTEnvironmentWrapper.MPSTSemanticWrapper
import mpst.frontend.auxiliary.wrappers.NetworkWrapper.{NetworkCausal, NetworkNonCausal}
import mpst.frontend.auxiliary.wrappers.SyncEnvironmentWrapper.SyncTraverseWrapper
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{FullMergeProjection, PlainMergeProjection}
import mpst.syntax.Protocol.*
import mpst.utility.Environment.{Environment, SingleEnvironment, localsEnvironment}
import mpst.utility.Multiset
import mpst.wellformedness.{WellBounded, WellBranched, WellChanneled}


case class Widgets(root: String):
  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map{ case k -> v => s"$k -> $v" }.mkString("\n")

  val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Message Sequence Chart"
      -> view(MessageSequenceChart.apply, Mermaid),

    "Global"
      -> view((global: Global) => s"${global.toString}", Code("java")),

    "Locals"
      -> view((global: Global) =>
      localsWithParticipant()(using global).map { case participant -> local => s"$participant: $local" }.mkString("\n"),
      Code("java")
    ).setRender(getSetting.allActiveFrom(root).exists(_.name == "Merge")),

    "Local Automata"
      -> viewMerms((global: Global) =>
      val environment = localsEnvironment(global)
      localsWithParticipant()(using global).map { case participant -> local =>
        val lts = caos.sos.SOS.toMermaid(
          MPSTSemanticWrapper,
          local -> environment(participant),
          (local: Local, environment: SingleEnvironment) => local.toString,
          _.toString,
          100,
        )
        participant -> lts
      }.toList
    ).setRender(getSetting.allActiveFrom(root).exists(_.name == "Merge")),

    "Local Compositional Automata - Synchronous"
      -> lts((global: Global) =>
      initialStateSync()(using global),
      SyncTraverseWrapper,
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Sync")),

    "Local Compositional Automata - Asynchronous (Causal)"
      -> lts((global: Global) =>
      initialStateAsyncCS()(using global),
      NetworkCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Causal)")),

    "Local Compositional Automata - Asynchronous (Non-Causal)"
      -> lts((global: Global) =>
      initialStateAsyncNCS()(using global),
      NetworkNonCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n"),
      _.toString,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Step-by-Step - Synchronous"
      -> steps((global: Global) =>
      initialStateSync()(using global),
      SyncTraverseWrapper,
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Sync")),

    "Step-by-Step Asynchronous (Causal)"
      -> steps((global: Global) =>
      initialStateAsyncCS()(using global),
      NetworkCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Causal)")),

    "Step-by-Step Asynchronous (Non-Causal)"
      -> steps((global: Global) =>
      initialStateAsyncNCS()(using global),
      NetworkNonCausal,
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Bisimulation - Sync vs Async (Causal)"
      -> compareBranchBisim(
      SyncTraverseWrapper,
      NetworkCausal,
      (global: Global) => initialStateSync()(using global),
      (global: Global) => initialStateAsyncCS()(using global),
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Causal)")),

    "Bisimulation - Sync vs Async (Non-Causal)"
      -> compareBranchBisim(
      SyncTraverseWrapper,
      NetworkNonCausal,
      (global: Global) => initialStateSync()(using global),
      (global: Global) => initialStateAsyncNCS()(using global),
      (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local "
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Bisimulation - Async (Causal) vs Async (Non-Causal)"
      -> compareBranchBisim(
      NetworkCausal,
      NetworkNonCausal,
      (global: Global) => initialStateAsyncCS()(using global),
      (global: Global) => initialStateAsyncNCS()(using global),
      (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case (participant, local) => s"$participant: $local"
      }.mkString("\n"),
      maxDepth = 100,
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Causal)") && getSetting.allActiveLeavesFrom(s"$root.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Well Channeled"
      -> check((global: Global) =>
      if !WellChanneled(global) then Seq(s"[$global] is not well channeled") else Seq.empty
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Extra Requirements").exists(_.name == "Well Channeled")),

    "Well Branched"
      -> check((global: Global) =>
      if !WellBranched(global) then Seq(s"[$global] is not well branched") else Seq.empty
    ).setRender(getSetting.allActiveLeavesFrom(s"$root.Extra Requirements").exists(_.name == "Well Branched")),

    "Well Bounded"
      -> check((global: Global) =>
      if !WellBounded(global) then Seq(s"[$global] is not well bounded") else Seq.empty
    ),
  )

  private def localsWithParticipant(enabledMerge: Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge"))(using global: Global): Set[(Participant, Local)] =
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

  private def initialStateSync(enabledMerge: Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge"))(using global: Global): (Set[(Participant, Local)], Option[Recv], Environment) =
    val initialStateOption = enabledMerge match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    allChecksLocals(initialState._1)
    initialState
  end initialStateSync

  private def initialStateAsyncCS(enabledMerge: Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge"))(using global: Global): (Set[(Participant, Local)], ChannelQueue, Environment) =
    val initialStateOption = enabledMerge match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
    allChecksLocals(initialState._1)
    initialState
  end initialStateAsyncCS

  private def initialStateAsyncNCS(enabledMerge: Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge"))(using global: Global): (Set[(Participant, Local)], Multiset[Action], Environment) =
    val initialStateOption = enabledMerge match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
    allChecksLocals(initialState._1)
    initialState
  end initialStateAsyncNCS

  private def allChecksLocals(localsWithParticipant: Set[(Participant, Local)]): Unit =
    checkLocals(localsWithParticipant, hasParallel, !getSetting.allActiveLeavesFrom(root).exists(_.name == "Parallel"), "Parallel")
    checkLocals(localsWithParticipant, hasKleeneStarRecursion, !getSetting.allActiveLeavesFrom(s"$root.Recursion").exists(_.name == "Kleene Star"), "Recursion Kleene Star")
    checkLocals(localsWithParticipant, hasFixedPointRecursion, !getSetting.allActiveLeavesFrom(s"$root.Recursion").exists(_.name == "Fixed Point"), "Recursion Fixed Point")
  end allChecksLocals

  private def checkLocals(localsWithParticipant: Set[(Participant, Local)], localCondition: Local => Boolean, settingCondition: => Boolean, prefix: String): Unit =
    localsWithParticipant.foreach {
      case participant -> local if localCondition(local) && settingCondition =>
        throw RuntimeException(s"$prefix - present on participant [$participant]")
      case _ =>
    }
  end checkLocals
end Widgets