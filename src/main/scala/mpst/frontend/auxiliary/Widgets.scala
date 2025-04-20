package mpst.frontend.auxiliary

import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Site.{getSetting, initSite}
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS
import caos.view.{Code, Mermaid}
import mpst.frontend.auxiliary.view.MessageSequenceChart
import mpst.frontend.auxiliary.wrappers.MPSTEnvironmentWrapper.MPSTSemanticWrapper
import mpst.frontend.auxiliary.wrappers.NetworkWrapper.{CausalState, NetworkCausal, NetworkNonCausal, NonCausalState}
import mpst.frontend.auxiliary.wrappers.SyncEnvironmentWrapper.{SyncState, SyncTraverseWrapper}
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
    "Well Channeled" ->
      check((global: Global) =>
        if !WellChanneled(global) then Seq(s"[$global] is not well channeled") else Seq.empty
      ).setRender(enabledExtraRequirements.exists(_.name == "Well Channeled")),

    "Well Branched" ->
      check((global: Global) =>
        if !WellBranched(global) then Seq(s"[$global] is not well branched") else Seq.empty
      ).setRender(enabledExtraRequirements.exists(_.name == "Well Branched")),

    "Well Bounded" ->
      check((global: Global) =>
        if !WellBounded(global) then Seq(s"[$global] is not well bounded") else Seq.empty
      ),

    "Message Sequence Chart" ->
      view(
        MessageSequenceChart.apply,
        Mermaid
      ),

    "Global" ->
      view((global: Global) =>
        s"${global.toString}",
        Code("java")
      ),

    "Locals" ->
      view((global: Global) =>
        localsWithParticipant(using global).map {
          case participant -> local => s"$participant: $local"
        }.mkString("\n"),
        Code("java")
      ).setRender(getSetting.allActiveFrom(root).exists(_.name == "Merge")),

    "Local Automata" ->
      viewMerms((global: Global) =>
        val environment: Environment = localsEnvironment(global)
        localsWithParticipant(using global).map { case participant -> local =>
          val lts: String = caos.sos.SOS.toMermaid(
            MPSTSemanticWrapper,
            local -> environment(participant),
            (local: Local, environment: SingleEnvironment) => local.toString,
            _.toString,
            100,
          )
          participant -> lts
        }.toList
      ).setRender(getSetting.allActiveFrom(root).exists(_.name == "Merge")),

    "Local Compositional Automata - Sync" ->
      lts(
        syncArguments._2,
        syncArguments._1,
        syncArguments._3,
        _.toString,
        100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Sync")),

    "Local Compositional Automata - Causal Async" ->
      lts(
        causalAsyncArguments._2,
        causalAsyncArguments._1,
        causalAsyncArguments._3,
        _.toString,
        100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Causal Async")),

    "Local Compositional Automata - Non-Causal Async" ->
      lts(
        nonCausalAsyncArguments._2,
        nonCausalAsyncArguments._1,
        nonCausalAsyncArguments._3,
        _.toString,
        100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Non-Causal Async")),

    "Step-by-Step - Sync" ->
      steps(
        syncArguments._2,
        syncArguments._1,
        syncArguments._3,
      ).setRender(enabledCommunicationModels.exists(_.name == "Sync")),

    "Step-by-Step - Causal Async" ->
      steps(
        causalAsyncArguments._2,
        causalAsyncArguments._1,
        causalAsyncArguments._3,
      ).setRender(enabledCommunicationModels.exists(_.name == "Causal Async")),

    "Step-by-Step - Non-Causal Async" ->
      steps(
        nonCausalAsyncArguments._2,
        nonCausalAsyncArguments._1,
        nonCausalAsyncArguments._3,
      ).setRender(enabledCommunicationModels.exists(_.name == "Non-Causal Async")),

    "Bisimulation - Sync vs Causal Async" ->
      compareBranchBisim(
        SyncTraverseWrapper,
        NetworkCausal,
        (global: Global) => initialStateSync(using global),
        (global: Global) => initialStateAsyncCS(using global),
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Sync") && enabledCommunicationModels.exists(_.name == "Causal Async")),

    "Bisimulation - Sync vs Non-Causal Async" ->
      compareBranchBisim(
        SyncTraverseWrapper,
        NetworkNonCausal,
        (global: Global) => initialStateSync(using global),
        (global: Global) => initialStateAsyncNCS(using global),
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Sync") && enabledCommunicationModels.exists(_.name == "Non-Causal Async")),

    "Bisimulation - Causal Async vs Non-Causal Async" ->
      compareBranchBisim(
        NetworkCausal,
        NetworkNonCausal,
        (global: Global) => initialStateAsyncCS(using global),
        (global: Global) => initialStateAsyncNCS(using global),
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(enabledCommunicationModels.exists(_.name == "Causal Async") && enabledCommunicationModels.exists(_.name == "Non-Causal Async")),
  )

  private def enabledCommunicationModels: Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Communication Model")
  private def enabledExtraRequirements:   Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Extra Requirements")
  private def enabledMerges:              Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge")
  private def enabledRecursions:          Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Recursion")

  private type State = SyncState | CausalState | NonCausalState
  private type Pending = Option[Recv] | ChannelQueue | Multiset[Action]

  private def getShowState(state: State): String = state.asInstanceOf[(Set[(Participant, Local)], Pending, Environment)] match
    case (localsWithParticipant, pending, environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
      case participant -> local => s"$participant: $local"
    }.mkString("\n")
  end getShowState

  private def syncArguments: (SOS[Action, State], Global => State, State => String) = (
    SyncTraverseWrapper.asInstanceOf[SOS[Action, State]],
    (global: Global) => initialStateSync(using global),
    getShowState,
  )
  end syncArguments

  private def causalAsyncArguments: (SOS[Action, State], Global => State, State => String) = (
    NetworkCausal.asInstanceOf[SOS[Action, State]],
    (global: Global) => initialStateAsyncCS(using global),
    getShowState,
  )
  end causalAsyncArguments

  private def nonCausalAsyncArguments: (SOS[Action, State], Global => State, State => String) = (
    NetworkNonCausal.asInstanceOf[SOS[Action, State]],
    (global: Global) => initialStateAsyncNCS(using global),
    getShowState,
  )

  private def localsWithParticipant(using global: Global): Set[(Participant, Local)] =
    val localsWithParticipantOption = enabledMerges match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some(PlainMergeProjection.projectionWithParticipant(global))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some(FullMergeProjection.projectionWithParticipant(global))
      case _ => None
    val localsWithParticipant = localsWithParticipantOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    checkLocalsAgainstAllConditions(localsWithParticipant)
    localsWithParticipant
  end localsWithParticipant

  private def initialStateSync(using global: Global): (Set[(Participant, Local)], Option[Recv], Environment) =
    val initialStateOption = enabledMerges match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    checkLocalsAgainstAllConditions(initialState._1)
    initialState
  end initialStateSync

  private def initialStateAsyncCS(using global: Global): (Set[(Participant, Local)], ChannelQueue, Environment) =
    val initialStateOption = enabledMerges match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
    checkLocalsAgainstAllConditions(initialState._1)
    initialState
  end initialStateAsyncCS

  private def initialStateAsyncNCS(using global: Global): (Set[(Participant, Local)], Multiset[Action], Environment) =
    val initialStateOption = enabledMerges match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
    checkLocalsAgainstAllConditions(initialState._1)
    initialState
  end initialStateAsyncNCS

  private def checkLocalsAgainstAllConditions(localsWithParticipant: Set[(Participant, Local)]): Unit =
    checkLocalsAgainstCondition(localsWithParticipant, hasParallel, !getSetting.allActiveLeavesFrom(root).exists(_.name == "Parallel"), "Parallel")
    checkLocalsAgainstCondition(localsWithParticipant, hasKleeneStarRecursion, !enabledRecursions.exists(_.name == "Kleene Star"), "Recursion Kleene Star")
    checkLocalsAgainstCondition(localsWithParticipant, hasFixedPointRecursion, !enabledRecursions.exists(_.name == "Fixed Point"), "Recursion Fixed Point")
  end checkLocalsAgainstAllConditions

  private def checkLocalsAgainstCondition(localsWithParticipant: Set[(Participant, Local)], localCondition: Local => Boolean, settingCondition: => Boolean, prefix: String): Unit =
    localsWithParticipant.foreach {
      case participant -> local if localCondition(local) && settingCondition =>
        throw RuntimeException(s"$prefix - present on participant [$participant]")
      case _ =>
    }
  end checkLocalsAgainstCondition
end Widgets