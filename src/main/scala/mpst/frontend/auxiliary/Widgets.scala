package mpst.frontend.auxiliary

import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Site.getSetting
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


case class Widgets(rootA: String, rootB: String):
  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map{ case k -> v => s"$k -> $v" }.mkString("\n")

  def sortedWidgets: Seq[(String, Option[WidgetInfo[Global]])] =
    val (semanticsA, semanticsB, others) = widgets.foldLeft(
      ( List.empty[(String, Option[WidgetInfo[Global]])],
        List.empty[(String, Option[WidgetInfo[Global]])],
        List.empty[(String, Option[WidgetInfo[Global]])] )
    ) {
      case ((sA, sB, o), widget @ (title, _)) =>
        if      title.startsWith("Semantics A:") then (widget :: sA, sB, o)
        else if title.startsWith("Semantics B:") then (sA, widget :: sB, o)
        else    (sA, sB, widget :: o)
    }
    others.reverse ++ semanticsA.reverse ++ semanticsB.reverse
  end sortedWidgets

  private def widgets: Seq[(String, Option[WidgetInfo[Global]])] = List(
    "Message Sequence Chart" -> Some(
      view(
        MessageSequenceChart.apply,
        Mermaid
      )
    ),

    "Global" -> Some(
      view((global: Global) =>
        s"${global.toString}",
        Code("java")
      )
    ),

    "Well Bounded" -> Some(
      check((global: Global) =>
        if !WellBounded(global) then Seq(s"[$global] is not well bounded") else Seq.empty
      )
    ),
  ) ++ mkWidgetsForAll(
    "Semantics A: Well Channeled",
    "Semantics B: Well Channeled",
    mkWellChanneled,
  ) ++ mkWidgetsForAll(
    "Semantics A: Well Branched",
    "Semantics B: Well Branched",
    mkWellBranched,
  ) ++ mkWidgetsForAll(
    "Semantics A: Locals",
    "Semantics B: Locals",
    mkLocals,
  ) ++ mkWidgetsForAll(
    "Semantics A: Local FSM",
    "Semantics B: Local FSM",
    mkLocalAutomata,
  ) ++ mkWidgetsForAll(
    "Semantics A: Local Compositional FSM",
    "Semantics B: Local Compositional FSM",
    mkLocalCompositionalAutomata,
  ) ++ mkWidgetsForAll(
    "Semantics A: Step-by-Step",
    "Semantics B: Step-by-Step",
    mkStepsBySteps,
  ) ++ mkWidgetsFromAll(
    "Bisimulation",
    mkBisimulation,
  )

  private def mkWidgetsForAll(nameA: String, nameB: String, mkWidget: (root: String) => Option[WidgetInfo[Global]]): Seq[(String, Option[WidgetInfo[Global]])] = List(
    nameA -> mkWidget(rootA),
    nameB -> mkWidget(rootB),
  )
  end mkWidgetsForAll

  private def mkWidgetsFromAll(name: String, mkWidget: (rootA: String, rooB: String) => Option[WidgetInfo[Global]]): Seq[(String, Option[WidgetInfo[Global]])] = List(
    name -> mkWidget(rootA, rootB),
  )
  end mkWidgetsFromAll

  private def mkWellChanneled(root: String): Option[WidgetInfo[Global]] = Option.when(enabledExtraRequirements(root).exists(_.name == "Well Channeled")) {
    check((global: Global) =>
      if !WellChanneled(global) then Seq(s"[$global] is not well channeled") else Seq.empty
    )
  }
  end mkWellChanneled

  private def mkWellBranched(root: String): Option[WidgetInfo[Global]] = Option.when(enabledExtraRequirements(root).exists(_.name == "Well Branched")) {
    check((global: Global) =>
      if !WellBranched(global) then Seq(s"[$global] is not well branched") else Seq.empty
    )
  }
  end mkWellBranched

  private def mkLocals(root: String): Option[WidgetInfo[Global]] = Option.when(getSetting.allActiveFrom(root).exists(_.name == "Merge")) {
    view((global: Global) =>
      localsWithParticipant(root)(using global).map {
        case participant -> local => s"$participant: ${local.toSimpleString}"
      }.mkString("\n"),
      Code("java")
    )
  }
  end mkLocals

  private def mkLocalAutomata(root: String): Option[WidgetInfo[Global]] = Option.when(getSetting.allActiveFrom(root).exists(_.name == "Merge")) {
    viewMerms((global: Global) =>
      val environment: Environment = localsEnvironment(global)
      localsWithParticipant(root)(using global).map { case participant -> local =>
        val lts: String = caos.sos.SOS.toMermaid(
          MPSTSemanticWrapper,
          local -> environment(participant),
          (local: Local, environment: SingleEnvironment) => local.toSimpleString,
          _.toSimpleString,
          100,
        )
        participant -> lts
      }.toList
    )
  }
  end mkLocalAutomata

  private def mkLocalCompositionalAutomata(root: String): Option[WidgetInfo[Global]] = Option.when(enabledCommunicationModels(root).nonEmpty) {
    val (semantics, initialState, showState) = getArguments(root, enabledCommunicationModels(root).head)
    lts(
      initialState,
      semantics,
      showState,
      _.toString,
      100
    )
  }
  end mkLocalCompositionalAutomata

  private def mkStepsBySteps(root: String): Option[WidgetInfo[Global]] = Option.when(enabledCommunicationModels(root).nonEmpty) {
    val (semantics, initialState, showState) = getArguments(root, enabledCommunicationModels(root).head)
    steps(
      initialState,
      semantics,
      showState
    )
  }
  end mkStepsBySteps

  private def mkBisimulation(rootA: String, rootB: String): Option[WidgetInfo[Global]] = Option.when(enabledCommunicationModels(rootA).nonEmpty && enabledCommunicationModels(rootB).nonEmpty) {
    val (semanticsA, initialStateA, showStateA) = getArguments(rootA, enabledCommunicationModels(rootA).head)
    val (semanticsB, initialStateB, showStateB) = getArguments(rootB, enabledCommunicationModels(rootB).head)
    compareBranchBisim(
      semanticsA,
      semanticsB,
      initialStateA,
      initialStateB,
      showStateA,
      showStateB,
      _.toString,
      100,
    )
  }
  end mkBisimulation

  private type State = SyncState | CausalState | NonCausalState
  private type Pending = Option[Recv] | ChannelQueue | Multiset[Action]

  private def getArguments(root: String, enabledCommunicationModel: Setting): (SOS[Action, State], Global => State, State => String) =
    def getShowState(state: State): String = state.asInstanceOf[(Set[(Participant, Local)], Pending, Environment)] match
      case (localsWithParticipant, pending, environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
        case participant -> local => s"$participant: $local"
      }.mkString("\n")
    end getShowState

    enabledCommunicationModel.name match
      case "Sync" => (
        SyncTraverseWrapper.asInstanceOf[SOS[Action, State]],
        (global: Global) => initialStateSync(root)(using global),
        getShowState,
      )
      case "Causal Async" => (
        NetworkCausal.asInstanceOf[SOS[Action, State]],
        (global: Global) => initialStateAsyncCS(root)(using global),
        getShowState,
      )
      case "Non-Causal Async" => (
        NetworkNonCausal.asInstanceOf[SOS[Action, State]],
        (global: Global) => initialStateAsyncNCS(root)(using global),
        getShowState,
      )
      case other => throw RuntimeException(s"unexpected communication model [$other] found")
  end getArguments

  private def localsWithParticipant(root: String)(using global: Global): Set[(Participant, Local)] =
    val localsWithParticipantOption = enabledMerges(root) match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some(PlainMergeProjection.projectionWithParticipant(global))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some(FullMergeProjection.projectionWithParticipant(global))
      case _ => None
    val localsWithParticipant = localsWithParticipantOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    checkLocalsAgainstAllConditions(root, localsWithParticipant)
    localsWithParticipant
  end localsWithParticipant

  private def initialStateSync(root: String)(using global: Global): (Set[(Participant, Local)], Option[Recv], Environment) =
    val initialStateOption = enabledMerges(root) match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
    checkLocalsAgainstAllConditions(root, initialState._1)
    initialState
  end initialStateSync

  private def initialStateAsyncCS(root: String)(using global: Global): (Set[(Participant, Local)], ChannelQueue, Environment) =
    val initialStateOption = enabledMerges(root) match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
    checkLocalsAgainstAllConditions(root, initialState._1)
    initialState
  end initialStateAsyncCS

  private def initialStateAsyncNCS(root: String)(using global: Global): (Set[(Participant, Local)], Multiset[Action], Environment) =
    val initialStateOption = enabledMerges(root) match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some((FullMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
      case _ => None
    val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
    checkLocalsAgainstAllConditions(root, initialState._1)
    initialState
  end initialStateAsyncNCS

  private def checkLocalsAgainstAllConditions(root: String, localsWithParticipant: Set[(Participant, Local)]): Unit =
    checkLocalsAgainstCondition(localsWithParticipant, hasParallel, !getSetting.allActiveLeavesFrom(root).exists(_.name == "Parallel"), "Parallel")
    checkLocalsAgainstCondition(localsWithParticipant, hasKleeneStarRecursion, !enabledRecursions(root).exists(_.name == "Kleene Star"), "Recursion Kleene Star")
    checkLocalsAgainstCondition(localsWithParticipant, hasFixedPointRecursion, !enabledRecursions(root).exists(_.name == "Fixed Point"), "Recursion Fixed Point")
  end checkLocalsAgainstAllConditions

  private def checkLocalsAgainstCondition(localsWithParticipant: Set[(Participant, Local)], localCondition: Local => Boolean, settingCondition: => Boolean, prefix: String): Unit =
    localsWithParticipant.foreach {
      case participant -> local if localCondition(local) && settingCondition =>
        throw RuntimeException(s"$prefix - present on participant [$participant]")
      case _ =>
    }
  end checkLocalsAgainstCondition

  private def enabledCommunicationModels(root: String): Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Communication Model")
  private def enabledExtraRequirements(root: String):   Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Extra Requirements")
  private def enabledMerges(root: String):              Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Merge")
  private def enabledRecursions(root: String):          Set[Setting] = getSetting.allActiveLeavesFrom(s"$root.Recursion")
end Widgets