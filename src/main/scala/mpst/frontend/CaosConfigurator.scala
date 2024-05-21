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
    "MasterWorkers" -> "m>wA:Work<void> ; m>wB:Work<void> ; (wA>m:Done<void> || wB>m:Done<void>)",
  )
  val widgets:Seq[(String,WidgetInfo[Protocol])] = List(
    "view parsed protocol"
      -> view(_.toString,Text),
    "well formedness"
      -> view(wellFormedness,Text),
    // @ telmo - not quite what I want
    "my spin on \"Choreo Semantics (without added dependencies for b-pomsets)\""
      -> steps(xs => getInitialState(xs:Protocol),MPSTSemanticWrapper,_.toString,typ=Text),
    "Global LTS"
      -> lts(xs => getInitialState(xs:Protocol),MPSTSemanticWrapper,viewSt = _ =>" "),
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

  // @ telmo - this will be problematic!
  private def getInitialState(protocol:Protocol):(Protocol,Map[Variable,Global]) =
    protocol -> Environment.globalEnv(protocol)
  end getInitialState

  // @ telmo - just to warp it
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