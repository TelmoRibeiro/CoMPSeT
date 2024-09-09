package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS
import caos.sos.SOS.*
import caos.view.*
import mpst.projection.{AsyncProjection, SyncProjection}
import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.utilities.Multiset
import mpst.wellformedness.*
import mpst.operational_semantic.{MPSTSemanticWrapper, NetworkMultisetWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.MPSTSemanticWrapper.*
import mpst.syntax.Keyword.{ComAsyncMS, ComSync, InterleaveOff, RecKleene}

object CaosConfigurator extends Configurator[Configuration]:
  // var usedProjection = Void
  // var usedSos        = Void
  // var usedRec        = Void
  // var usedInter      = Void

  override val name:String = "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName:String = "session"

  override val parser:String=>Configuration = parseAllWrapper

  override val examples:Seq[Example] = List(
    "MasterWorkers"
      -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",

    "SimpleRecursion"
      -> "def X in (m>w:Task ; X)"
  )

  override val options:Seq[Option] = List(
    "Communication Type" -> (AsyncMS,AsyncCS,Sync,Multicast),
    "Interleaving"       -> (On,Off),
    "Recursion"          -> (FPTailRec,KleeneClosue,Off),
  )

  override val widgets:Seq[(String,WidgetInfo[Configuration])] = List(
    "parsed configuration"
      -> view(
      viewProg = (config:Configuration) =>
        val global -> keywords = config
        s"parsed global: ${global.toString}\nparsed config: ${keywords.toString}",
      typ = Text),

    "well formedness"
      -> view(
      viewProg = (config:Configuration) =>
        val global -> _ = config
        s"${wellFormedness(global)}",
      typ = Text),

    // @ telmo - async not properly working
    "my spin on \"Choreo Semantics \""
      -> steps(
      initialSt = (config:Configuration) =>
        val global -> _ = config
        global -> Environment.globalEnv(global),
      sos       = MPSTSemanticWrapper,
      viewSt    = (pro:Protocol,env:Map[Variable,Protocol]) => pro.toString,
      typ       = Text,
    ),

    "Composed Local MSNet Semantics - lazy view"
      -> steps(
      initialSt = (config:Configuration) =>
        val global -> configuration = config
        val locals   = AsyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        (locals,Multiset(),localEnv),
      sos    = NetworkMultisetWrapper,
      viewSt = (loc:Set[(Agent,Local)],pen:Multiset[Action],env:Map[Agent,Map[Variable,Local]]) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ    = Text,
    ),

    "Composed Local Sync Semantics - lazy view"
      -> steps(
      initialSt = (config:Configuration) =>
        val global -> configuration = config
        val locals      = SyncProjection.projectionWithAgent(global)
        val localEnv    = Environment.localEnv(global)
        locals -> localEnv,
      sos    = SyncTraverseWrapper,
      viewSt = (loc:Set[(Agent,Local)],env:LocalEnv) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ    = Text,
    ),

    "Global LTS - with lazy environment"
      -> lts(
      initialSt = (config:Configuration) =>
        val global -> _ = config
        global -> Environment.globalEnv(global),
      sos       = MPSTSemanticWrapper,
      viewSt    = (global:Global,environment:Map[Variable,Global]) => environment.toString,
    ),

    "Local LTS - with lazy environment"
     -> viewMerms((config:Configuration) =>
      val result = for agent -> local <- AsyncProjection.projectionWithAgent(config._1) yield
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

  private def parseAllWrapper(input:String):Configuration =
    Parser.parseConfiguration(input)
  end parseAllWrapper

  // @ telmo - expand this to better error handling
  // @ telmo - "check" might be useful here
  private def wellFormedness(global:Global):String =
    if DependentlyGuarded(global) && WellBounded(global) && WellBranched(global) && WellChannelled(global) && WellCommunicated(global)
    then "WELL FORMED!"
    else "NOT WELL FORMED!"
  end wellFormedness
end CaosConfigurator