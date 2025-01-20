package mpst.frontend

import mpst.frontend.MessageSequenceChart.*
import mpst.frontend.caos_wrapper.{MPSTSemanticWrapper, NetworkWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.Network.NetworkCausal.ChannelQueue
import mpst.projection.{PlainMergeProjection, StandardProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Protocol.{Action, Global, Local, Participant, Variable, hasFixedPointRecursion, hasInterleaving, hasKleeneStarRecursion, toString}
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

  /* @ telmo -
    simple description -
      standard name convention
    example:
    override val setting: Setting = ("Sync" || "Async MS" || "Async CS") && "Interleaving"
    */

  /* @ telmo -
    multi-config description -
      testing if the syntax is simple enough
    example:
    private def mkConfig: Setting = "Comm Model" -> ("Sync" || "Async CS" || "Async MS") && "Interleaving"
    override val setting: Setting = "Configurations" -> ("ConfigA" -> mkConfig || "ConfigB" -> mkConfig)
    */

  /* @ telmo -
    "complex" description -
      renaming names
    example:
    */

  override val setting: Setting = "Configuration" -> ("Comm Model" -> ("Sync" || "Async MS" || "Async CS") && "Interleaving" && "Recursion" -> ("Fixed Point" || "Kleene Star") && "Merge" -> ("Full" || "Plain"))

  override val examples: Seq[Example] = List(
    "AsyncCS vs AsyncMS"
      -> "(m>w:Work || m>w:WorkAgain) ; w>m:Done",

    "MasterWorkers"
      -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",

    "Plain Merge"
      -> "m>wA:Work ; wC>m:Done + m>wB:Work ; wC>m:DoneA",

    "SimpleRecursion"
      -> "def X in (m>w:Task ; X)",
  )

  extension [K, V](map: Map[K, V])
    private def toPrettyPrint: String = map.map{case k -> v => s"$k -> $v"}.mkString("\n")

  private def mkCheck(condition: Protocol => Boolean, term: String): WidgetInfo[Global] =
    check((global: Global) =>
      (Seq("global" -> global) ++ StandardProjection.projectionWithParticipant(global)).collect {
        case participant -> protocol if condition(protocol) => s"$participant: found [$term] in [$protocol]"
      }
    )
  end mkCheck

  override val widgets: Seq[(String, WidgetInfo[Global])] = List(
    "Message Sequence Chart"
      -> view(MessageSequenceChart.apply, Mermaid),

    "Global"
      -> view((global: Global) => s"${global.toString}", Code("java")),

    "Well Formedness"
      -> check((global: Global) =>
        def wellFormed(condition: Global => Boolean): Seq[String] =
          if !condition(global) then Seq(s"[$global] failed while testing [$condition]") else Seq.empty
        end wellFormed

        wellFormed(DependentlyGuarded.apply) ++ wellFormed(WellBounded.apply) ++ wellFormed(WellBranched.apply) ++ wellFormed(WellChannelled.apply) ++ wellFormed(WellCommunicated.apply)
      ),

    "Locals"
      -> view((global: Global) =>
        val localsWithParticipant = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case mergeOptions if mergeOptions.exists(_.name == "Plain") =>
            Some(PlainMergeProjection.projectionWithParticipant(global))
          case mergeOptions if mergeOptions.exists(_.name == "Full") =>
            Some(StandardProjection.projectionWithParticipant(global))
          case _ => None
        localsWithParticipant.getOrElse(throw RuntimeException("Merge - checked yet no options enabled")).map{ case participant -> local => s"$participant -> $local" }.mkString("\n"),
        Code("java")
      ).setRender(getSetting.allActiveFrom("Configuration").exists(_.name == "Merge")),

    "\"Choreo\" - My Spin"
      -> steps((global: Global) =>
        global -> globalEnvironment(global),
        MPSTSemanticWrapper,
        (global: Global, environment: SingleEnvironment) => global.toString,
        _.toString,
        Text
      ),

    "Global Automata"
      -> lts((global: Global) =>
        global -> globalEnvironment(global),
        MPSTSemanticWrapper,
        (global: Global, environment: SingleEnvironment) => environment.toPrettyPrint,
      ),

    "Local Automata"
     -> viewMerms((global: Global) =>
        val environment = localsEnvironment(global)
        val localsWithParticipant = getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case mergeOptions if mergeOptions.exists(_.name == "Plain") =>
            Some(PlainMergeProjection.projectionWithParticipant(global))
          case mergeOptions if mergeOptions.exists(_.name == "Full") =>
            Some(StandardProjection.projectionWithParticipant(global))
          case _ => None
        localsWithParticipant.getOrElse(throw RuntimeException("Merge - checked yet no options enabled")).map{ case participant -> local =>
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
     ).setRender(getSetting.allActiveFrom("Configuration").exists(_.name == "Merge")),

    "Conditional Sync"
      -> steps((global: Global) =>
        getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case mergeOptions if mergeOptions.exists(_.name == "Plain") =>
            PlainMergeProjection.projectionWithParticipant(global) -> localsEnvironment(global)
          case _ =>
            StandardProjection.projectionWithParticipant(global) -> localsEnvironment(global),
        SyncTraverseWrapper.Traverse,
        (localsWithParticipant: Set[(Participant, Local)], environment: Environment) => localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Sync")),

    "Conditional Async CS"
      -> steps((global: Global) =>
        getSetting.allActiveLeavesFrom("Configuration.Merge") match
          case mergeOptions if mergeOptions.exists(_.name == "Plain") =>
            (PlainMergeProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global))
          case _ =>
            (StandardProjection.projectionWithParticipant(global), Map.empty, localsEnvironment(global)),
        NetworkWrapper.NetworkCausal,
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async CS")),

    "Conditional Async MS"
      -> steps((global: Global) =>
      getSetting.allActiveLeavesFrom("Configuration.Merge") match
        case mergeOptions if mergeOptions.exists(_.name == "Plain") =>
          (PlainMergeProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global))
        case _ =>
          (StandardProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)),
        NetworkWrapper.NetworkMultiset,
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
      ).setRender(getSetting.allActiveLeavesFrom("Configuration.Comm Model").exists(_.name == "Async MS")),

    "Conditional Kleene Start Checker"
      -> mkCheck(hasKleeneStarRecursion, "Kleene Star Recursion").setRender(!getSetting.allActiveLeavesFrom("Configuration.Recursion").exists(_.name == "Kleene Star")),

    "Conditional Fixed Point Checker"
      -> mkCheck(hasFixedPointRecursion, "Fixed Point Recursion").setRender(!getSetting.allActiveLeavesFrom("Configuration.Recursion").exists(_.name == "Fixed Point")),

    "Conditional Interleaving Checker"
      -> mkCheck(hasInterleaving, "Interleaving").setRender(!getSetting.allActiveLeavesFrom("Configuration").exists(_.name == "Interleaving")),

  // need to re-render after this
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

    "Bisimulation - ..."
      -> compareBranchBisim(
        NetworkWrapper.NetworkCausal,
        NetworkWrapper.NetworkMultiset,
        (global: Global) => (StandardProjection.projectionWithParticipant(global), Map.empty,  localsEnvironment(global)),
        (global: Global) => (StandardProjection.projectionWithParticipant(global), Multiset(), localsEnvironment(global)),
        (localsWithParticipant: Set[(Participant, Local)], pending: ChannelQueue, environment: Environment) => localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        (localsWithParticipant: Set[(Participant, Local)], pending: Multiset[Action], environment: Environment) => localsWithParticipant.map {
          case (participant, local) => s"$participant: $local "
        }.mkString("\n"),
        maxDepth = 100,
    ),
   */
  )
end CaosConfigurator