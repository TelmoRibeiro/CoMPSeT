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
import mpst.syntax.Protocol.*
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.wellformedness.*

import mpst.operational_semantic.MPSTSemanticWrapper
import mpst.operational_semantic.MPSTSemanticWrapper.*

object CaosConfigurator extends Configurator[Protocol]:
  val name = "To Be Named..."

  override val languageName:String = "Protocol"

  val parser:String => Protocol = justParseIt

  val examples:Seq[Example] = List(
    "MasterWorkers" -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",
  )
  val widgets:Seq[(String,WidgetInfo[Protocol])] = List(
    "view parsed protocol"
      -> view(_.toString,Text),
    "well formedness"
      -> view(wellFormedness,Text),
    // @ telmo - not quite what I want CHECK CURRENT PROBLEM
    "my spin on \"Choreo Semantics (without added dependencies for b-pomsets)\""
      -> steps(global => getInitialState(global:Global),MPSTSemanticWrapper,(protocol:Protocol,environment:Map[Variable,Protocol])=>protocol.toString,typ=Text),
    "Global LTS - lazy Env"
      -> lts(global => getInitialState(global),MPSTSemanticWrapper,viewSt = (protocol:Protocol,environment:Map[Variable,Protocol])=>environment.toString),
    "Local LTS"
     -> viewMerms((protocol:Protocol) =>
      val result = for agent -> local <- AsyncProjection.projectionWithAgent(protocol) yield
        agent -> caos.sos.SOS.toMermaid(
          MPSTSemanticWrapper,
          local -> Environment.singleLocalEnv(local),
          _ => " ",
          _.toString, 80
        )
      result.toList
      )
  )

  // @ telmo - will this be problematic?
  private def getInitialState(global:Global):(Protocol,Map[Variable,Global]) =
    global -> Environment.globalEnv(global)
  end getInitialState

  // @ telmo - too hacky?
  private def justParseIt(dsl:String):Protocol =
    Parser(dsl)
  end justParseIt

  // @ telmo - expand this to better error handling
  private def wellFormedness(global:Global):String =
    if DependentlyGuarded(global) && WellBounded(global) && WellBranched(global) && WellChannelled(global) && WellCommunicated(global)
    then "WELL FORMED!"
    else "NOT WELL FORMED!"
  end wellFormedness
end CaosConfigurator