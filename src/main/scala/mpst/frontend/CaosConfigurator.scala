package mpst.frontend

import mpst.frontend.MessageSequenceChart.*
import mpst.frontend.caos_wrapper.MPSTEnvironmentWrapper.MPSTSemanticWrapper
import mpst.frontend.caos_wrapper.NetworkWrapper.{NetworkCausal, NetworkMultiset}
import mpst.frontend.caos_wrapper.SyncEnvironmentWrapper.SyncTraverseWrapper
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{PlainMergeProjection, StandardProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.{Receive, Action, Global, Local, Participant, Variable, hasFixedPointRecursion, hasKleeneStarRecursion, hasParallel, toString}
import mpst.utility.Environment.{Environment, SingleEnvironment, globalEnvironment, localsEnvironment}
import mpst.utility.Multiset
import mpst.wellformedness.*
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.Setting.{allActiveFrom, allActiveLeavesFrom}
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS.toMermaid
import caos.view.{Code, Mermaid, Text}

import scala.collection.immutable.Queue
import scala.language.implicitConversions
import caos.frontend.Site.{getSetting, setSetting}

/* @ telmo
  IDEA:
    => instantiate CAOS.Configurator
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object CaosConfigurator extends Configurator[Global]:
  override val name: String =
    "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName: String =
    "Session"

  override val parser: String => Global =
    (input: String) => Parser(input)

  override val setting: Setting = "Configuration" -> ("Merge" -> ("Plain" || "Full") && "Comm Model" -> ("Sync" && "Async (Causal)" && "Async (Non-Causal)") && "Recursion" -> ("Kleene Star" || "Fixed Point") && "Parallel" && "Extra Requirements" -> ("Well Branched" && "Well Channeled" && "Well Bounded"))

  // paperA: GentleSync
  private val paperA: Setting = setting
    .setAllChecked("Configuration.Merge.Full")
    .setAllChecked("Configuration.Comm Model.Sync")
    .setAllChecked("Configuration.Recursion.Fixed Point")
    .setAllChecked("Configuration.Extra Requirements.Well Branched")
    .setAllChecked("Configuration.Extra Requirements.Well Bounded")

  // paperB: GentleAsync (new async)
  private val paperB: Setting = setting
    .setAllChecked("Configuration.Merge.Plain")
    .setAllChecked("Configuration.Comm Model.Async (Causal)")
    .setAllChecked("Configuration.Recursion.Fixed Point")
    .setAllChecked("Configuration.Extra Requirements.Well Branched")

  // paperC: APIGenScala
  private val paperC: Setting = setting
    .setAllChecked("Configuration.Merge.Plain")
    .setAllChecked("Configuration.Comm Model.Async (Causal)")
    .setAllChecked("Configuration.Parallel")
    .setAllChecked("Configuration.Extra Requirements.Well Branched")
    .setAllChecked("Configuration.Extra Requirements.Well Channeled")

  // paperD: ST4MP
  private val paperD: Setting = setting
    .setAllChecked("Configuration.Merge.Plain")
    .setAllChecked("Configuration.Comm Model.Async (Causal)")
    .setAllChecked("Configuration.Parallel")
    .setAllChecked("Configuration.Recursion.Kleene Star")
    .setAllChecked("Configuration.Extra Requirements.Well Branched")
    .setAllChecked("Configuration.Extra Requirements.Well Channeled")

  // paperE: ...
  private val paperE: Setting = setting

  // "AsyncCS vs AsyncMS"
  //      -> "(m->w:Work || m->w:WorkAgain) ; w->m:Done"
  //      -> setting,
  //
  //    "Plain Merge"
  //      -> "m->wA:Work ; wC->m:Done + m->wB:Work ; wC->m:DoneA"
  //      -> setting,

  private val mwv1: String = "m->wA:Work ; m->wB:Work ;\nwA->m:Done ; wB->m:Done"

  private val mwv2: String = "m->wA:Work ; m->wB:Work ;\n(wA->m:Done || wB->m:Done)"

  private val mwv3: String = "def X in (\n\tm->w:Work ; w->m:Done ; X + m->w:Quit\n)"

  private val mwv4: String = "def X in (\n\t(\n\t\tm->wA:Work; wA->wB:Work ;\n\t\t(wA->m:Done || wB->m:Done) ; X\n\t) + (m->wA:Quit; wA->wB:Quit)\n)"

  private val mwv5: String = "(\n\tm->wA:Work ; m->wB:Work ;\n\t(wA->m:Done || wB->m:Done)\n)*"

  override val examples: Seq[Example] = List(
    "MW-v1"
      -> mwv1
      -> "fully sequentialized master-worker (no settings)"
      -> setting,

    "MW-v2"
      -> mwv2
      -> "standard master-worker (no settings)"
      -> setting,

    "MW-v3"
      -> mwv3
      -> "fully sequentialized master-worker with fixed point recursion (no setting)"
      -> setting,

    "MW-v4"
      -> mwv4
      -> "parallel master-worker protocol with fixed point recursion (no setting)"
      -> setting,

    "MW-v5"
      -> mwv5
      -> "standard master-worker with kleene star recursion (no setting)"
      -> setting,

    "MW-v2 (APIGenInScala3)"
      -> mwv2
      -> "MW-v2 under the APIGenInScala3 settings"
      -> paperC,

    "MW-v3 (VeryGentleIntroMPST)"
      -> mwv3
      -> "MW-v3 under the VeryGentleIntroMPST settings"
      -> paperA,

    "MW-v3 (GentleIntroMPAsyncST)"
      -> mwv3
      -> "MW-v3 under the GentleIntroMPAsyncST settings"
      -> paperB,

    "MW-v5 (ST4MP)"
      -> mwv5
      -> "MW-v5 under the ST4MP settings"
      -> paperD,
  )

  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map{case k -> v => s"$k -> $v"}.mkString("\n")

  private def localsWithParticipant(enabledMerge: Set[Setting])(using global: Global): Set[(Participant, Local)] =
    val localsWithParticipantOption = enabledMerge match
      case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
        Some(PlainMergeProjection.projectionWithParticipant(global))
      case enabledMerge if enabledMerge.exists(_.name == "Full") =>
        Some(StandardProjection.projectionWithParticipant(global))
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
    checkLocals(localsWithParticipant, hasParallel, !getSetting.allActiveLeavesFrom("Configuration").exists(_.name == "Parallel"), "Parallel")
    checkLocals(localsWithParticipant, hasKleeneStarRecursion, !getSetting.allActiveLeavesFrom("Configuration.Recursion").exists(_.name == "Kleene Star"), "Recursion Kleene Star")
    checkLocals(localsWithParticipant, hasFixedPointRecursion, !getSetting.allActiveLeavesFrom("Configuration.Recursion").exists(_.name == "Fixed Point"), "Recursion Fixed Point")
  end allChecksLocals

  override val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Message Sequence Chart"
      -> view(MessageSequenceChart.apply, Mermaid),

    "Global"
      -> view((global: Global) => s"${global.toString}", Code("java")),

    "Locals"
      -> view((global: Global) =>
        val localsWithParticipantOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some(PlainMergeProjection.projectionWithParticipant(global))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some(StandardProjection.projectionWithParticipant(global))
          case _ => None
        val localsWithParticipant = localsWithParticipantOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
        allChecksLocals(localsWithParticipant)
        localsWithParticipant.map{ case participant -> local => s"$participant -> $local" }.mkString("\n"),
        Code("java")
      ).setRender(getSetting.allActiveFrom("Configuration").exists(_.name == "Merge")),

    "Local Compositional Automata - Synchronous"
      -> lts((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
        allChecksLocals(initialState._1)
        initialState,
        SyncTraverseWrapper,
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Receive], environment: Environment) => "",
        _.toString,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Sync")),

    "Local Compositional Automata - Asynchronous (Causal)"
      -> lts((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
        allChecksLocals(initialState._1)
        initialState,
        NetworkCausal,
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => "",
        _.toString,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Causal)")),

    "Local Compositional Automata - Asynchronous (Non-Causal)"
      -> lts((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
        allChecksLocals(initialState._1)
        initialState,
        NetworkMultiset,
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => "",
        _.toString,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Step-by-Step - Synchronous"
      -> steps((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), None, localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled"))
        allChecksLocals(initialState._1)
        initialState,
        SyncTraverseWrapper,
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Receive], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Sync")),

    "Step-by-Step Asynchronous (Causal)"
      -> steps((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], ChannelQueue, Environment)]
        allChecksLocals(initialState._1)
        initialState,
        NetworkCausal,
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Causal)")),

    "Step-by-Step Asynchronous (Non-Causal)"
      -> steps((global: Global) =>
        val initialStateOption = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case enabledMerge if enabledMerge.exists(_.name == "Plain") =>
            Some((PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
          case enabledMerge if enabledMerge.exists(_.name == "Full") =>
            Some((StandardProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)))
          case _ => None
        val initialState = initialStateOption.getOrElse(throw RuntimeException("Merge - some option must be enabled")).asInstanceOf[(Set[(Participant, Local)], Multiset[Action], Environment)]
        allChecksLocals(initialState._1)
        initialState,
        NetworkMultiset,
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Sync vs Async (Causal) - Bisimulation"
      -> compareBranchBisim(
        SyncTraverseWrapper,
        NetworkCausal,
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), None, localsEnvironment(global)),
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), Map.empty, localsEnvironment(global)),
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Receive], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Causal)")),

    "Sync vs Async (Non-Causal) - Bisimulation"
      -> compareBranchBisim(
        SyncTraverseWrapper,
        NetworkMultiset,
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), None, localsEnvironment(global)),
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), Multiset(), localsEnvironment(global)),
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Receive], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Sync") && getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Async (Causal) vs Async (Non-Causal) - Bisimulation"
      -> compareBranchBisim(
        NetworkCausal,
        NetworkMultiset,
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), Map.empty, localsEnvironment(global)),
        (global: Global) => (localsWithParticipant(getSetting.allActiveLeavesFrom("Configuration.Merge"))(using global), Multiset(), localsEnvironment(global)),
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map {
          case (participant, local) => s"$participant: $local"
        }.mkString("\n"),
        maxDepth = 100,
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Causal)") && getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async (Non-Causal)")),

    "Extra Requirements" // fix this
      -> check((global: Global) =>
        if !DependentlyGuarded(global)  then Seq(s"[$global] is not dependently guarded") else Seq.empty
      ).setRender(getSetting.allActiveLeavesFrom("Configuration").exists(_.name == "Extra Requirements")),

    "Well Channeled"
      -> check((global: Global) =>
        if !WellChanneled(global) then Seq(s"[$global] is not well channeled") else Seq.empty
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Extra Requirements").exists(_.name == "Well Channeled")),

    "Well Bounded"
      -> check((global: Global) =>
        if !WellBounded(global) then Seq(s"[$global] is not well bounded") else Seq.empty
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Extra Requirements").exists(_.name == "Well Bounded")),

    "Well Branched"
      -> check((global:Global) =>
        if !WellBranched(global) then Seq(s"[$global] is not well branched") else Seq.empty
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Extra Requirements").exists(_.name == "Well Branched")),

    /*
    "Dynamic Setting Test"
      -> check((global: Global) => Site.getSetting match
        case Some(setting) if
          setting.getChecked("Configuration.Comm Model").getOrElse(false) &&
          setting.getChecked("Configuration.Interleaving").getOrElse(false) &&
          setting.resolvePath("Configuration.New Option").isEmpty =>
          Site.setSetting("Configuration" -> (setting && "New Option"))
          Seq()
        case _ =>
          Seq()
      ),
    */
  )
end CaosConfigurator