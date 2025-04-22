package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.Setting
import caos.frontend.widgets.WidgetInfo
import mpst.frontend.auxiliary.{Examples, Widgets}
import mpst.syntax.Parser
import mpst.syntax.Protocol.Global


object CaosConfigurator extends Configurator[Global]:
  override val name: String = "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName: String = "Session"

  override val parser: String => Global = (input: String) => Parser(input)

  private val root:  String = "Semantics"
  private val rootA: String = "Semantics A"
  private val rootB: String = "Semantics B"

  private def mkSemantics: Setting =
    "Merge" -> ("Plain" || "Full") && "Communication Model" -> ("Sync" || "Causal Async" || "Non-Causal Async") && "Recursion" -> ("Kleene Star" || "Fixed Point") && "Parallel" && "Extra Requirements" -> ("Well Branched" && "Well Channeled")
  end mkSemantics

  override val setting: Setting = Setting(root, List(s"$rootA" -> mkSemantics, s"$rootB" -> mkSemantics), options = List("allowAll"))

  override val examples: Seq[Example] = Examples(setting, s"$root.$rootA", s"$root.$rootB").examples

  override def widgets: Seq[(String, WidgetInfo[Global])] =
    sortWidgets(Widgets(s"$root.$rootA", s"$root.$rootB").widgets)
  end widgets

  private def sortWidgets(widgets: Seq[(String, WidgetInfo[Global])]) =
    val (semanticsA, semanticsB, others) = widgets.foldLeft(
      ( List.empty[(String, WidgetInfo[Global])],
        List.empty[(String, WidgetInfo[Global])],
        List.empty[(String, WidgetInfo[Global])] )
    ) {
      case ((sA, sB, o), widget @ (title, _)) =>
        if      title.startsWith("Semantics A:") then (widget :: sA, sB, o)
        else if title.startsWith("Semantics B:") then (sA, widget :: sB, o)
        else    (sA, sB, widget :: o)
    }
    others.reverse ++ semanticsA.reverse ++ semanticsB.reverse
  end sortWidgets
end CaosConfigurator