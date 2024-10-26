package mpst.frontend

import mpst.frontend.caos_wrapper.{MPSTSemanticWrapper, NetworkWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{AsyncProjection, SyncProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.{Action, Global, Local, Participant, Variable, toString}
import mpst.utility.Environment.{Environment, SingleEnvironment, globalEnvironment, localsEnvironment}
import mpst.utility.Multiset
import mpst.wellformedness.*

import caos.frontend.Configurator
import caos.frontend.Configurator.{check, lts, steps, view, viewMerms, Example, Setting, SettingCondition}
import caos.frontend.widgets.WidgetInfo
import caos.sos.SOS.toMermaid
import caos.view.{Code, Text}

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
        SyncProjection.projectionWithAgent(global) -> localsEnvironment(global),
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
        (AsyncProjection.projectionWithAgent(global), Map.empty, localsEnvironment(global)),
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
        (AsyncProjection.projectionWithAgent(global), Multiset(), localsEnvironment(global)),
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
      "def X in (m>w:Task ; X)",
  )

  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map {
      case k -> v => s"$k -> $v"
    }.mkString("\n")

  override val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Global" ->
      view(
        viewProg = (global: Global) => s"${global.toString}",
        typ      = Code("java")
      ),

    "Well Formedness" ->
      check((global: Global) =>
        def wellFormed(condition: Global => Boolean): Seq[String] =
          if !condition(global) then Seq(s"[$global] failed while testing [$condition]") else Seq.empty
        end wellFormed

        wellFormed(DependentlyGuarded.apply) ++ wellFormed(WellBounded.apply) ++ wellFormed(WellBranched.apply) ++ wellFormed(WellChannelled.apply) ++ wellFormed(WellCommunicated.apply)
      ),

    "Locals" ->
      view(
        viewProg = (global: Global) =>
          AsyncProjection.projectionWithAgent(global).map {
            case participant -> local => s"$participant -> $local"
          }.mkString("\n"),
        typ = Code("java")
      ),

    "\"Choreo\" - My Spin" ->
      steps(
        initialSt = (global: Global) =>
          global -> globalEnvironment(global),
        sos    = MPSTSemanticWrapper,
        viewSt = (global: Global, environment: SingleEnvironment) =>
          global.toString,
        typ = Text
    ),

    "Global Automata"
      -> lts(
      initialSt = (global: Global) =>
        global -> globalEnvironment(global),
      sos = MPSTSemanticWrapper,
      viewSt = (global: Global, environment: SingleEnvironment) =>
        environment.toPrettyPrint,
    ),

    "Local Automata"
     -> viewMerms((global: Global) =>
        val environment = localsEnvironment(global)
        AsyncProjection.projectionWithAgent(global).map {
          case participant -> local =>
            val lts = caos.sos.SOS.toMermaid(
              sos = MPSTSemanticWrapper,
              s = local -> environment(participant),
              showSt = (local: Local, environment: SingleEnvironment) =>
                environment.toPrettyPrint,
              showAct = _.toString,
              maxNodes = 100,
            )
            participant -> lts
        }.toList
    ),
  )
end CaosConfigurator