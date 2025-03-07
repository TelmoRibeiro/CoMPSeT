package mpst.frontend

import mpst.frontend.MessageSequenceChart.*
import mpst.frontend.caos_wrapper.MPSTEnvironmentWrapper.MPSTSemanticWrapper
import mpst.frontend.caos_wrapper.NetworkWrapper.{NetworkCausal, NetworkMultiset}
import mpst.frontend.caos_wrapper.SyncEnvironmentWrapper.SyncTraverseWrapper
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{PlainMergeProjection, FullMergeProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.{Recv, Action, Global, Local, Participant, Variable, hasFixedPointRecursion, hasKleeneStarRecursion, hasParallel, toString}
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


object CaosConfigurator extends Configurator[Global]:
  override val name: String =
    "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName: String =
    "Session"

  override val parser: String => Global =
    (input: String) => Parser(input)

  private def mkSemantics: Setting = "Merge" -> ("Plain" || "Full") && "Comm Model" -> ("Sync" && "Async (Causal)" && "Async (Non-Causal)") && "Recursion" -> ("Kleene Star" || "Fixed Point") && "Parallel" && "Extra Requirements" -> ("Well Branched" && "Well Channeled")
  override val setting: Setting = "Semantics" -> mkSemantics

  private val semanticsA: String = "Semantics"

  private def mkVeryGentleIntroMPST(setting: Setting, prefix: String): Setting = setting
    .setCheckedPath(s"$prefix.Merge.Full", true)
    .setCheckedPath(s"$prefix.Comm Model.Sync", true)
    .setCheckedPath(s"$prefix.Recursion.Fixed Point", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Branched", true)
  end mkVeryGentleIntroMPST

  private def mkGentleIntroMPAsyncST(setting: Setting, prefix: String): Setting = setting
    .setCheckedPath(s"$prefix.Merge.Plain", true)
    .setCheckedPath(s"$prefix.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$prefix.Recursion.Fixed Point", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Branched", true)
  end mkGentleIntroMPAsyncST

  private def mkAPIGenInScala3(setting: Setting, prefix: String): Setting = setting
    .setCheckedPath(s"$prefix.Merge.Plain", true)
    .setCheckedPath(s"$prefix.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$prefix.Parallel", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Channeled", true)
  end mkAPIGenInScala3

  private def mkST4MP(setting: Setting, prefix: String): Setting = setting
    .setCheckedPath(s"$prefix.Merge.Plain", true)
    .setCheckedPath(s"$prefix.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$prefix.Parallel", true)
    .setCheckedPath(s"$prefix.Recursion.Kleene Star", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$prefix.Extra Requirements.Well Channeled", true)
  end mkST4MP

  private val simpleDelegation: String = "m->w:TaskA || m->w:TaskB"

  private val simpleBranchingV1: String = "(wA->wB:TaskA ; wB->m:DoneA)\n\t+\n(wA->wB:TaskB ; wB->m:DoneA)"

  private val simpleBranchingV2: String = "(wA->wB:TaskA ; wB->m:DoneA)\n\t+\n(wA->wB:TaskB ; wB->m:DoneB)"

  private val mwv0: String = "m->wA:Work ; m->wB:Work ;\nwA->m:Done ; wB->m:Done"

  private val mwv1: String = "m->wA:Work ; m->wB:Work ;\n(wA->m:Done || wB->m:Done)"

  private val mwv2: String = "(\n\tm->wA:Work ; m->wB:Work ;\n\t(wA->m:Done || wB->m:Done)\n)*"

  private val recursiveMasterWorker: String = "def X in (\n\tm->w:Work ; w->m:Done ; X + m->w:Quit\n)"

  private val badWellBranched: String = "(m->wA:Work ; wA->m:Done)\n\t+\n(m->wB:Work ; wB->m:Done)"

  private val badWellChannelled: String = "(m->w:TaskA ; w->m:Done)\n\t||\n(m->w:TaskB ; w->m:Done)"

  override val examples: Seq[Example] = List(
    "master-workers - v1"
      -> mwv1
      -> "standard master-workers (no settings)"
      -> setting,

    "master-workers - v0"
      -> mwv0
      -> "fully sequentialized master-workers (no settings)"
      -> setting,

    "master-workers - v2"
      -> mwv2
      -> "standard master-workers under kleene star recursion (no settings)"
      -> setting,

    "simple task delegation"
      -> simpleDelegation
      -> "a simple task delegation using parallel composition (no settings)"
      -> setting,

    "simple branching - v1"
      -> simpleBranchingV1
      -> "a simple branching protocol - plain-merge (no settings)"
      -> setting,

    "simple branching - v2"
      -> simpleBranchingV2
      -> "a simple branching protocol - full-merge (no settings)"
      -> setting,

    "master-worker - fixed point recursion"
      -> recursiveMasterWorker
      -> "sequentialized master-worker with fixed point recursion (no settings)"
      -> setting,

    "APIGenInScala3 settings"
      -> mwv0
      -> "APIGenInScala3 settings (placeholder protocol)"
      -> mkAPIGenInScala3(setting, semanticsA),

    "ST4MP settings"
      -> mwv0
      -> "ST4MP settings (placeholder protocol)"
      -> mkST4MP(setting, semanticsA),

    "VeryGentleIntroMPST settings"
      -> mwv0
      -> "VeryGentleIntroMPST settings (placeholder protocol)"
      -> mkVeryGentleIntroMPST(setting, semanticsA),

    "GentleIntroMPAsyncST settings"
      -> mwv0
      -> "GentleIntroMPAsyncST settings (placeholder protocol)"
      -> mkGentleIntroMPAsyncST(setting, semanticsA),

    "master-workers - v1 (APIGenInScala3)"
      -> mwv1
      -> "master-workers-v1 under the APIGenInScala3 settings"
      -> mkAPIGenInScala3(setting, semanticsA),

    "master-workers - v2 (ST4MP)"
      -> mwv2
      -> "master-workers-v2 under the ST4MP settings"
      -> mkST4MP(setting, semanticsA),

    "simple branching - v1 (GentleIntroMPAsyncST)"
      -> simpleBranchingV1
      -> "simple branching - v1 under the GentleIntroMPAsyncST"
      -> mkGentleIntroMPAsyncST(setting, semanticsA),

    "simple branching - v2 (VeryGentleIntroMPST)"
      -> simpleBranchingV2
      -> "simple branching - v2 under the VeryGentleIntroMPST settings"
      -> mkVeryGentleIntroMPST(setting, semanticsA),

    "simple task delegation (APIGenInScala3 vs Non-Causal Async.)"
      -> simpleDelegation
      -> "simple delegation under the APIGenInScala3 settings vs non-causal async. communication"
      -> mkAPIGenInScala3(setting,semanticsA).setCheckedPath("Semantics.Comm Model.Async (Non-Causal)", true),

    "master-worker - fixed point recursion (ST4MP) | recursion fail"
      -> recursiveMasterWorker
      -> "failed recursion for the master-worker - fixed point recursion under ST4MP settings"
      -> mkST4MP(setting, semanticsA),

    "master-workers - v2 (GentleIntroMPAsyncST) | parallel fail"
      -> mwv2
      -> "failed parallel for the master-workers - v2 under GentleIntroMPAsyncST settings"
      -> mkGentleIntroMPAsyncST(setting, semanticsA),
  )

  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map{case k -> v => s"$k -> $v"}.mkString("\n")

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

  override val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Message Sequence Chart"
      -> view(MessageSequenceChart.apply, Mermaid),

    "Global"
      -> view((global: Global) => s"${global.toString}", Code("java")),

    "Locals"
      -> view((global: Global) =>
        localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global).map{ case participant -> local => s"$participant -> $local" }.mkString("\n"),
        Code("java")
      ).setRender(getSetting.allActiveFrom("Semantics").exists(_.name == "Merge")),

    "Local Automata"
      -> viewMerms((global: Global) =>
        val environment = localsEnvironment(global)
        localsWithParticipant(getSetting.allActiveLeavesFrom("Semantics.Merge"))(using global).map{ case participant -> local =>
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
        (localsWithParticipant: Set[(Participant, Local)], pendingReceive: Option[Recv], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map{
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
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map{
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
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.toSeq.sortBy(_._1).map{
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
end CaosConfigurator