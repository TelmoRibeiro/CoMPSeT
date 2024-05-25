package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS
import caos.sos.SOS.*
import caos.view.*

import mpst.projection.AsyncProjection

import mpst.syntax.Parser
import mpst.syntax.Protocol
import mpst.syntax.Type.*

import mpst.utilities.Environment

import mpst.wellformedness.*

import mpst.operational_semantic.MPSTSemanticWrapper
import mpst.operational_semantic.MPSTSemanticWrapper.*

object CaosConfigurator extends Configurator[Configuration]:
  override val name:String = "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName:String = "protocol"

  override val parser:String=>Configuration = parseAllWrapper

  override val examples:Seq[Example] = List(
    "MasterWorkers"
      -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",
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
    "my spin on \"Choreo Semantics (without added dependencies for b-pomsets)\""
      -> steps(
      initialSt = (config:Configuration) =>
        val global -> _ = config
        global -> Environment.globalEnv(global),
      sos       = MPSTSemanticWrapper,
      viewSt    = (pro:Protocol,env:Map[Variable,Protocol]) => pro.toString,
      typ       = Text,
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

  private def parseWrapper(input:String):Protocol =
    Parser(input)
  end parseWrapper

  // @ telmo - expand this to better error handling
  private def wellFormedness(global:Global):String =
    if DependentlyGuarded(global) && WellBounded(global) && WellBranched(global) && WellChannelled(global) && WellCommunicated(global)
    then "WELL FORMED!"
    else "NOT WELL FORMED!"
  end wellFormedness
end CaosConfigurator