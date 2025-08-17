package mpst.frontend.auxiliary

import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Site.getSetting
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS
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
import mpst.wellformedness.{KleeneStarProjectable, WellBounded, WellBranched, WellChanneled}

case class Widgets(rootA: String, rootB: String):
  private val rA = rootA.replace("Semantics.", "")
  private val rB = rootB.replace("Semantics.", "")

  def sortedWidgets: Seq[(String, Option[WidgetInfo[Global]])] =
    val (semanticsA, restA) = widgets.partition(_._1.startsWith(s"$rA:"))
    val (semanticsB, restB) = restA.partition(_._1.startsWith(s"$rB:"))
    restB ++ semanticsA ++ semanticsB
  end sortedWidgets

  private def widgets: Seq[(String, Option[WidgetInfo[Global]])] =
    List(
      "Message Sequence Chart" -> Some(
        view(MessageSequenceChart.apply, Mermaid)
      ),
      "Global" -> Some(
        view((global: Global) => s"$global", Code("java"))
      ),
      "Well Bounded" -> Some(
        check((global: Global) =>
          if WellBounded(global) then Seq.empty else Seq.empty
        )
      ),
      "Kleene Star Projectable" -> Some(
        check((global: Global) =>
          if KleeneStarProjectable(global) then Seq.empty else Seq.empty
        )
      ),
      s"$rA: Well Channeled"          -> mkWellChanneled(rootA),
      s"$rB: Well Channeled"          -> mkWellChanneled(rootB),
      s"$rA: Well Branched"           -> mkWellBranched(rootA),
      s"$rB: Well Branched"           -> mkWellBranched(rootB),
      s"$rA: Locals"                  -> mkLocals(rootA),
      s"$rB: Locals"                  -> mkLocals(rootB),
      s"$rA: Local FSMs"              -> mkLocalFSMs(rootA),
      s"$rB: Local FSMs"              -> mkLocalFSMs(rootB),
      s"$rA: Local Compositional FSM" -> mkLocalCompositionalFSM(rootA),
      s"$rB: Local Compositional FSM" -> mkLocalCompositionalFSM(rootB),
      s"$rA: Iterator"                -> mkIterator(rootA),
      s"$rB: Iterator"                -> mkIterator(rootB),
      s"Bisimulation Checker"            -> mkBisimulationChecker(rootA, rootB)
    )
  end widgets

  private def mkWellChanneled(root: String): Option[WidgetInfo[Global]] =
    Option.when(has(root, "Extra Requirements", "Well Channeled")) {
      check((global: Global) => if WellChanneled(global) then Seq.empty else Seq.empty)
    }
  end mkWellChanneled

  private def mkWellBranched(root: String): Option[WidgetInfo[Global]] =
    Option.when(has(root, "Extra Requirements", "Well Branched")) {
      check((global: Global) => if WellBranched(global) then Seq.empty else Seq.empty)
    }
  end mkWellBranched

  private def mkLocals(root: String): Option[WidgetInfo[Global]] =
    Option.when(has(root, "Merge Criteria", "Plain") || has(root, "Merge Criteria", "Full")) {
      view((global: Global) =>
        localsWithParticipant(root)(using global)
          .toSeq.sortBy(_._1)
          .map { case participant -> local => s"$participant: ${local.toSimpleString}" }
          .mkString("\n"),
        Code("java")
      )
    }
  end mkLocals

  private def mkLocalFSMs(root: String): Option[WidgetInfo[Global]] =
    Option.when(has(root, "Merge Criteria", "Plain") || has(root, "Merge Criteria", "Full")) {
      viewMerms((global: Global) =>
        val env: Environment = localsEnvironment(global)
        localsWithParticipant(root)(using global)
          .toSeq.sortBy(_._1)
          .map { case participant -> local =>
            val lts: String = caos.sos.SOS.toMermaid(
              MPSTSemanticWrapper,
              local -> env(participant),
              (local: Local, _: SingleEnvironment) => local.toSimpleString,
              _.toSimpleString,
              100
            )
            participant -> lts
          }.toList
      )
    }
  end mkLocalFSMs

  private def mkLocalCompositionalFSM(root: String): Option[WidgetInfo[Global]] =
    enabledCommunicationModels(root).headOption.map {
      communicationModel =>
        val arguments = getArguments(root, communicationModel)
        lts(arguments.initialState, arguments.semantics, arguments.showState, _.toString, 100)
    }
  end mkLocalCompositionalFSM

  private def mkIterator(root: String): Option[WidgetInfo[Global]] =
    enabledCommunicationModels(root).headOption.map {
      communicationModel =>
        val arguments = getArguments(root, communicationModel)
        steps(arguments.initialState, arguments.semantics, arguments.showState)
    }
  end mkIterator

  private def mkBisimulationChecker(rootA: String, rootB: String): Option[WidgetInfo[Global]] =
    for
      communicationModelA <- enabledCommunicationModels(rootA).headOption
      communicationModelB <- enabledCommunicationModels(rootB).headOption
    yield
      val argumentsA = getArguments(rootA, communicationModelA)
      val argumentsB = getArguments(rootB, communicationModelB)
      compareBranchBisim(
        argumentsA.semantics,    argumentsB.semantics,
        argumentsA.initialState, argumentsB.initialState,
        argumentsA.showState,    argumentsB.showState,
        _.toString,
        100
      )
  end mkBisimulationChecker

  // --------
  // Wrappers
  // --------
  private case class Arguments[State](
    semantics: SOS[Action, State],
    initialState: Global => State,
    showState: State => String
  )

  private def getArguments(root: String, communicationModelSetting: Setting): Arguments[?] =
    def showState[State](state: State): String = state match
      case (locals: Set[(Participant, Local)] @unchecked, _pending, _env) =>
        locals
          .toSeq.sortBy(_._1)
          .map { case participant -> local => s"$participant: $local" }
          .mkString("\n")
      case _ => state.toString

    val communicationModel = CommunicationModel.fromSetting(communicationModelSetting)
      .fold(msg => throw RuntimeException(msg), identity)

    communicationModel match
      case CommunicationModel.Synchronous =>
        Arguments(
          SyncTraverseWrapper,
          (global: Global) => initialStateSync(root)(using global),
          showState
        )
      case CommunicationModel.OrderedAsynchronous =>
        Arguments(
          NetworkCausal,
          (global: Global) => initialStateAsyncOrdered(root)(using global),
          showState
        )
      case CommunicationModel.UnorderedAsynchronous =>
        Arguments(
          NetworkNonCausal,
          (global: Global) => initialStateAsyncUnordered(root)(using global),
          showState
        )
  end getArguments

  // --------------
  // Initial States
  // --------------
  private def initialStateSync(root: String)(using global: Global): (Set[(Participant, Local)], Option[Recv], Environment) =
    val locals -> env = localsAndEnv(root)
    (locals, None, env)
  end initialStateSync

  private def initialStateAsyncOrdered(root: String)(using global: Global): (Set[(Participant, Local)], ChannelQueue, Environment) =
    val locals -> env = localsAndEnv(root)
    (locals, Map.empty, env)
  end initialStateAsyncOrdered

  private def initialStateAsyncUnordered(root: String)(using global: Global): (Set[(Participant, Local)], Multiset[Action], Environment) =
    val locals -> env = localsAndEnv(root)
    (locals, Multiset(), env)
  end initialStateAsyncUnordered

  private def localsAndEnv(root: String)(using global: Global): (Set[(Participant, Local)], Environment) =
    val locals: Set[(Participant, Local)] = mergeCriteriaOrDie(root) match
      case MergeCriteria.Plain => PlainMergeProjection.projectionWithParticipant(global)
      case MergeCriteria.Full  => FullMergeProjection.projectionWithParticipant(global)
    checkAllConditions(root, locals)
    locals -> localsEnvironment(global)
  end localsAndEnv

  // ------
  // Checks
  // ------
  private def checkAllConditions(root: String, locals: Set[(Participant, Local)]): Unit =
    checkCondition("Parallel Composition",
          locals,
          hasParallel,
          enabled(root, "Parallel Composition").isEmpty
    )
    checkCondition("Kleene Star",
          locals,
          hasKleeneStarRecursion,
          !has(root, "Recursion Scheme", "Kleene Star")
    )
    checkCondition("Fixed Point",
          locals,
          hasFixedPointRecursion,
          !has(root, "Recursion Scheme", "Fixed Point")
    )
  end checkAllConditions

  private def checkCondition(prefix: String,
                    locals: Set[(Participant, Local)],
                    localCondition: Local => Boolean,
                    settingBlock: => Boolean
                   ): Unit =
    locals.foreach {
      case participant -> local if localCondition(local) && settingBlock =>
        throw RuntimeException(s"$prefix - present on participant [$participant]")
      case _ => ()
    }
  end checkCondition

  // -----------
  // Projections
  // -----------
  private def localsWithParticipant(root: String)(using global: Global): Set[(Participant, Local)] =
    val (locals, _) = localsAndEnv(root)
    locals
  end localsWithParticipant


  // ----------
  // UI Options
  // ----------
  sealed trait CommunicationModel
  private object CommunicationModel:
    case object Synchronous           extends CommunicationModel
    case object OrderedAsynchronous   extends CommunicationModel
    case object UnorderedAsynchronous extends CommunicationModel

    def fromSetting(enabled: Setting): Either[String, CommunicationModel] = enabled.name match
      case "Synchronous"            => Right(Synchronous)
      case "Ordered Asynchronous"   => Right(OrderedAsynchronous)
      case "Unordered Asynchronous" => Right(UnorderedAsynchronous)
      case _                        => Left("no valid [Communication Model] was selected")
    end fromSetting
  end CommunicationModel

  sealed trait MergeCriteria
  private object MergeCriteria:
    case object Plain extends MergeCriteria
    case object Full  extends MergeCriteria

    def fromSetting(enabled: Setting): Either[String, MergeCriteria] = enabled.name match
      case "Plain" => Right(Plain)
      case "Full"  => Right(Full)
      case _       => Left("no valid [Merge Criteria] was selected")
    end fromSetting
  end MergeCriteria

  // ---------------
  // setting helpers
  // ---------------
  private def enabledMergeCriteria(root: String): Set[Setting] =
    enabled(root, "Merge Criteria")
  end enabledMergeCriteria

  private def enabledCommunicationModels(root: String): Set[Setting] =
    enabled(root, "Communication Model")
  end enabledCommunicationModels

  private def enabledRecursionSchemes(root: String): Set[Setting] =
    enabled(root, "Recursion Scheme")
  end enabledRecursionSchemes

  private def enabledExtraRequirements(root: String): Set[Setting] =
    enabled(root, "Extra Requirements")
  end enabledExtraRequirements

  private def mergeCriteriaOrDie(root: String): MergeCriteria =
    MergeCriteria.fromSetting(enabledMergeCriteria(root).head).fold(msg => throw RuntimeException(msg), identity)
  end mergeCriteriaOrDie

  private def enabled(root: String, group: String): Set[Setting] =
    getSetting.allActiveLeavesFrom(s"$root.$group")
  end enabled

  private def has(root: String, group: String, item: String): Boolean =
    enabled(root, group).exists(_.name == item)
  end has
end Widgets