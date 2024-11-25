package mpst.frontend

import mpst.frontend.caos_wrapper.{MPSTSemanticWrapper, NetworkWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.StandardProjection
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
import scala.language.implicitConversions

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
  // simple description
  // override val setting: Setting = (Setting("Sync") || Setting("Async MS") || Setting("Async CS")) ++ Setting("Interleaving")
  // renamed description
  override val setting: Setting = "Configuration" -> (("Comm Model" -> (Setting("Sync") || Setting("Async MS") || Setting("Async CS"))) ++ Setting("Interleaving"))
  //********** SETTINGS DEFINITION **********//

  //********** OPTIONS DEFINITION **********//
  private val conditionalWidgets: Map[String, WidgetInfo[Global]] = Map(
    "Sync" ->
      steps(
        initialSt = (global: Global) =>
          StandardProjection.projectionWithParticipant(global) -> localsEnvironment(global),
        sos = SyncTraverseWrapper.Traverse,
        viewSt = (localsWithParticipant: Set[(Participant, Local)], environment: Environment) =>
          localsWithParticipant.map {
            case (participant, local) => s"$participant: $local "
          }.mkString("\n"),
        typ = Text
      ),

    "Async CS" ->
      steps(
        initialSt = (global: Global) =>
          (StandardProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)),
        sos = NetworkWrapper.NetworkCausal,
        viewSt = (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) =>
          localsWithParticipant.map {
            case (participant, local) => s"$participant: $local "
          }.mkString("\n"),
        typ = Text
      ),

    "Async MS" ->
      steps(
        initialSt = (global: Global) =>
          (StandardProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)),
        sos = NetworkWrapper.NetworkMultiset,
        viewSt = (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) =>
          localsWithParticipant.map {
            case (participant, local) => s"$participant: $local "
          }.mkString("\n"),
        typ = Text,
      ),

    "No Interleaving" ->
      check(
        (global: Global) =>
          def hasInterleaving(protocol: Protocol): Seq[String] =
            if Protocol.hasInterleaving(protocol) then Seq(s"interleaving construct found in $protocol") else Seq.empty
          end hasInterleaving

          hasInterleaving(global) ++ StandardProjection.projectionWithParticipant(global).flatMap(localWithParticipant => hasInterleaving(localWithParticipant._2)).toSeq
      ),
  )

  private def mkConditionalWidget(names: List[String]): List[(String, WidgetInfo[Global])] = {
    names.collect{ name => name -> conditionalWidgets(name) }
  }

  implicit def toNameList(name: String): List[String] = List(name)

  override val settingConditions: Seq[SettingCondition[Global]] = List(
    ((setting: Setting) =>  setting("Configuration.Comm Model.Sync"))     -> mkConditionalWidget("Sync"),
    ((setting: Setting) =>  setting("Configuration.Comm Model.Async CS")) -> mkConditionalWidget("Async CS"),
    ((setting: Setting) =>  setting("Configuration.Comm Model.Async MS")) -> mkConditionalWidget("Async MS"),
    ((setting: Setting) => !setting("Configuration.Interleaving"))        -> mkConditionalWidget("No Interleaving"),
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
          StandardProjection.projectionWithParticipant(global).map {
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
        StandardProjection.projectionWithParticipant(global).map {
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