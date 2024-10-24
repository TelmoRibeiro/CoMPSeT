package mpst.frontend

import mpst.frontend.caos_wrapper.{MPSTSemanticWrapper, NetworkWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{AsyncProjection, SyncProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.{Action, Global, Local, Participant, Variable, toString}
import mpst.utilities.Environment.{Environment, localEnv, globalEnv, singleLocalEnv}
import mpst.utilities.Multiset
import mpst.wellformedness.*

import caos.frontend.Configurator
import caos.frontend.Configurator.{check, lts, steps, view, viewMerms, Example, Setting, SettingCondition}
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS.toMermaid
import caos.view.Text

import scala.collection.immutable.Queue

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

  //********** SETTINGS DEFINITION **********//
  private def mkInterleavingOption =
    Setting(name = "Interleaving", render = true)
  end mkInterleavingOption

  private def mkCommModelOption =
    val syncChoice    = Setting(name = "Sync",     render = true)
    val asyncCSChoice = Setting(name = "Async CS", render = true)
    val asyncMSChoice = Setting(name = "Async MS", render = true)

    Setting("Comm Model", List(syncChoice, asyncCSChoice, asyncMSChoice), true)
  end mkCommModelOption

  private val ConfigA = Setting("Config A", List(mkInterleavingOption, mkCommModelOption), true)
  private val ConfigB = Setting("Config B", List(mkInterleavingOption, mkCommModelOption), true)

  override val setting: Setting = Setting("Settings", List(ConfigA, ConfigB), true)
  //********** SETTINGS DEFINITION **********//

  //********** OPTIONS DEFINITION **********//
  private def mkNoInterleavingWidget =
    check(
      (global: Global) =>
        def hasInterleaving(protocol: Protocol): Seq[String] =
          if Protocol.hasInterleaving(protocol) then Seq(s"interleaving construct found in $protocol") else Seq.empty
        end hasInterleaving

        hasInterleaving(global) ++ AsyncProjection.projectionWithAgent(global).flatMap(localWithParticipant => hasInterleaving(localWithParticipant._2)).toSeq
    )
  end mkNoInterleavingWidget

  private def mkSyncWidget =
    steps(
      initialSt = (global: Global) =>
        SyncProjection.projectionWithAgent(global) -> localEnv(global),
      sos       = SyncTraverseWrapper.Traverse,
      viewSt    = (localsWithParticipant: Set[(Participant, Local)], environment: Environment) =>
        localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      typ       = Text
    )
  end mkSyncWidget

  private def mkAsyncCSWidget =
    steps(
      initialSt = (global: Global) =>
        (AsyncProjection.projectionWithAgent(global), Map.empty, localEnv(global)),
      sos       = NetworkWrapper.NetworkCausal,
      viewSt    = (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) =>
        localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      typ       = Text
    )
  end mkAsyncCSWidget

  private def mkAsyncMSWidget =
    steps(
      initialSt = (global: Global) =>
        (AsyncProjection.projectionWithAgent(global), Multiset(), localEnv(global)),
      sos       = NetworkWrapper.NetworkMultiset,
      viewSt    = (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) =>
        localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      typ       = Text,
    )
  end mkAsyncMSWidget

  override val settingConditions: List[SettingCondition[Global]] = List(
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config A.Comm Model.Sync"),
      List("Sync A" -> mkSyncWidget)
    ),
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config B.Comm Model.Sync"),
      List("Sync B" -> mkSyncWidget)
    ),
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config A.Comm Model.Async CS"),
      List("Async CS A" -> mkAsyncCSWidget)
    ),
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config B.Comm Model.Async CS"),
      List("Async CS B" -> mkAsyncCSWidget)
    ),
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config A.Comm Model.Async MS"),
      List("Async MS A" -> mkAsyncMSWidget)
    ),
    SettingCondition(
      (setting: Setting) => setting.toMap("Settings.Config B.Comm Model.Async MS"),
      List("Async MS B" -> mkAsyncMSWidget)
    ),
    SettingCondition(
      (setting: Setting) => !setting.toMap("Settings.Config A.Interleaving"),
      List("No Interleaving A" -> mkNoInterleavingWidget)
    ),
    SettingCondition(
      (setting: Setting) => !setting.toMap("Settings.Config B.Interleaving"),
      List("No Interleaving B" -> mkNoInterleavingWidget)
    )
  )
  //********** OPTIONS DEFINITION **********//

  override val examples: Seq[Example] = List(
    "AsyncCS vs AsyncMS" ->
      "(m>w:Work || m>w:WorkAgain) ; w>m:Done",

    "MasterWorkers" ->
      "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",

    "SimpleRecursion" ->
      "def X in (m>w:Task ; X)"
  )
  
  override val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "parsed global" ->
      view(
        viewProg = (global: Global) =>
          s"parsed global: ${global.toString}",
        typ      = Text
      ),

    "well formedness" ->
      check((global: Global) =>
        def wellFormed(condition: Global => Boolean): Seq[String] =
          if !condition(global) then Seq(s"[$global] failed while testing [$condition]") else Seq.empty
        end wellFormed

        wellFormed(DependentlyGuarded.apply) ++ wellFormed(WellBounded.apply) ++ wellFormed(WellBranched.apply) ++ wellFormed(WellChannelled.apply) ++ wellFormed(WellCommunicated.apply)
      ),

    "my spin on \"Choreo Semantics \"" ->
      steps(
        initialSt = (global: Global) =>
          global -> globalEnv(global),
        sos       = MPSTSemanticWrapper,
        viewSt    = (protocol: Protocol, environment: Map[Variable, Protocol]) =>
          protocol.toString,
        typ       = Text
    ),

    "Global LTS - with lazy environment"
      -> lts(
      initialSt = (global: Global) =>
        global -> globalEnv(global),
      sos = MPSTSemanticWrapper,
      viewSt = (global: Global, environment: Map[Variable, Global]) =>
        environment.toString,
    ),

    "Local LTS - with lazy environment"
     -> viewMerms((global: Global) =>
        AsyncProjection.projectionWithAgent(global).map {
          case (participant, local) =>
            participant -> caos.sos.SOS.toMermaid(
              sos      = MPSTSemanticWrapper,
              s        = local -> singleLocalEnv(local),
              showSt   = (protocol: Protocol, environment: Map[Variable, Protocol]) =>
                environment.toString,
              showAct  = _.toString,
              maxNodes = 100,
            )
        }.toList
    ),
  )
end CaosConfigurator