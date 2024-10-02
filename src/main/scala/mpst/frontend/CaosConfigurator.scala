package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS
import caos.sos.SOS.*
import caos.view.*
import mpst.projection.{AsyncProjection, SyncProjection}
import mpst.syntax.{Parser, Protocol}
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.utilities.Multiset
import mpst.wellformedness.*
import mpst.operational_semantic.{MPSTSemanticWrapper, NetworkMultisetWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.MPSTSemanticWrapper.*

object CaosConfigurator extends Configurator[Global]:
  override val name:String = "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName:String = "Session"

  override val parser:String=>Global = (input:String) => Parser(input)

  //********** SETTING DEFINITION **********//
  private val AsyncMSWidget =
    "Composed Local MSNet Semantics"
      -> steps(
      initialSt = (global: Global) =>
        val locals = AsyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        (locals, Multiset(), localEnv),
      sos = NetworkMultisetWrapper,
      viewSt = (loc: Set[(Agent, Local)], pen: Multiset[Action], env: Map[Agent, Map[Variable, Local]]) =>
        loc.map { case (agent, local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    )

  private val AsyncMSOption = Option(Map("Async MS" -> true), List(AsyncMSWidget))

  override val options: List[Option[Global]] = List(AsyncMSOption)

  private def mkInterleavingOption = {
    Setting(name = "Interleaving", render = true)
  }

  private def mkCommModelOption = {
    val asyncMSChoice = Setting(name = "Async MS", render = true)
    val asyncCSChoice = Setting(name = "Async CS", render = true)
    val syncChoice    = Setting(name = "Sync",     render = true)
    Setting(name = "Comm Model", children = List(asyncMSChoice, asyncCSChoice, syncChoice), render = true)
  }

  private val ConfigA = Setting(name = "Config A", children = List(mkInterleavingOption, mkCommModelOption), render = true)
  private val ConfigB = Setting(name = "Config B", children = List(mkInterleavingOption, mkCommModelOption), render = true)

  override val setting: Setting = Setting(name = "Settings", children = List(ConfigA, ConfigB), render = true)
  //********** SETTING DEFINITION **********//

  override val examples:Seq[Example] = List(
    "MasterWorkers"
      -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",

    "SimpleRecursion"
      -> "def X in (m>w:Task ; X)"
  )
  
  override val widgets:Seq[(String,WidgetInfo[Global])] = List(
    "parsed global"
      -> view(
      viewProg = (global:Global) =>
        s"parsed global: ${global.toString}",
      typ = Text),

    "well formedness" // there is a check() caos function I might want to explore
      -> view(
      viewProg = (global:Global) =>
        s"${wellFormedness(global)}",
      typ = Text),

    // @ telmo - async not properly working
    "my spin on \"Choreo Semantics \""
      -> steps(
      initialSt = (global:Global) =>
        global -> Environment.globalEnv(global),
      sos       = MPSTSemanticWrapper,
      viewSt    = (pro:Protocol,env:Map[Variable,Protocol]) => pro.toString,
      typ       = Text,
    ),

    "Composed Local MSNet Semantics - lazy view"
      -> steps(
      initialSt = (global:Global) =>
        val locals   = AsyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        (locals,Multiset(),localEnv),
      sos = NetworkMultisetWrapper,
      viewSt = (loc:Set[(Agent,Local)],pen:Multiset[Action],env:Map[Agent,Map[Variable,Local]]) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    ),

    "Composed Local Sync Semantics - lazy view"
      -> steps(
      initialSt = (global:Global) =>
        val locals   = SyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        locals -> localEnv,
      sos = SyncTraverseWrapper,
      viewSt = (loc:Set[(Agent,Local)],env:LocalEnv) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    ),

    "Global LTS - with lazy environment"
      -> lts(
      initialSt = (global:Global) =>
        global -> Environment.globalEnv(global),
      sos = MPSTSemanticWrapper,
      viewSt = (global:Global,environment:Map[Variable,Global]) => environment.toString,
    ),

    "Local LTS - with lazy environment"
     -> viewMerms((global:Global) =>
      val result = for agent -> local <- AsyncProjection.projectionWithAgent(global) yield
        agent -> caos.sos.SOS.toMermaid(
          sos      = MPSTSemanticWrapper,
          s        = local -> Environment.singleLocalEnv(local),
          showSt   = (protocol:Protocol,environment:Map[Variable,Protocol]) => environment.toString,
          showAct  = _.toString,
          maxNodes = 100,
        )
      result.toList
      )
  )

  // @ telmo - expand this to better error handling
  // @ telmo - "check" might be useful here
  private def wellFormedness(global:Global):String =
    if DependentlyGuarded(global) && WellBounded(global) && WellBranched(global) && WellChannelled(global) && WellCommunicated(global)
    then "WELL FORMED!"
    else "NOT WELL FORMED!"
  end wellFormedness
end CaosConfigurator

/* BRAINSTORM:
  -- BOTTOM UP DESIGN --
    choice A = (choiceName, widgetsList, render) =>
      a tuple associating a name, the widgets necessary to implement said choice, and a boolean stating if it should be rendered
      ex: asyncMSChoice = ("Async - MultiSet", List(steps(...)), true)

    option A = (optionName, choiceList, render) =>
      a tuple associating a name, the choices available for said option, and a boolean stating if it should be rendered
      ex: communicationOption = ("Communication Model", List(asyncMSChoice, asyncCSChoice, syncChoice), true)

    config A = (configName, optionList, render) =>
      a tuple associating a name, the options available for said config, and a boolean stating if it should be rendered
      ex: config1 = ("Config A", List(communicationOption, InterleavingOption, ...), true)

    setting A = List(config) =>
      a list (tuple if binary) of configurations
      we can override it within the CaosConfigurator
      caos builds the UI knowing this structure
      the "render" arguments reflects on checkboxes the final user can enable or not (similar to the already present collapse system)
        essentially collapsing large sections to eliminate visual pollution
      there would be a "reload" option the final user could press after checking/unchecking boxes

      PROBLEMS:
        - is this to much?
          although it reflects almost 1:1 the visual structure as seen by the final user
            there are many categories: setting, config, options, choices, widgets
          the render choice will allow to prune entire sections when parsing the structure
          but the person implementing this structure in his caos configurator will need to write a lot...
            defining widgets, to define choices, to define options, to define configs, to alas define a setting that the caos will deal with
        - are there any problems that I am not foreseeing with implementing this on caos?
        - I am using lists of stuff for the "sake" of simplicity, the possibility of this being implemented in something
          like a tree is not lost on me, but then what about the different "tiers"
*/