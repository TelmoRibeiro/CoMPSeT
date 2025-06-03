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
    "Merge Criteria" -> ("Plain" || "Full") && "Communication Model" -> ("Synchronous" || "Causal Asynchronous" || "Non-Causal Asynchronous") && "Parallel Composition" && "Recursion" -> ("Fixed Point" || "Kleene Star") && "Extra Requirements" -> ("Well Branched" && "Well Channeled")
  end mkSemantics

  override val setting: Setting = Setting(root, List(s"$rootA" -> mkSemantics, s"$rootB" -> mkSemantics), options = List("allowAll"))

  override val examples: Seq[Example] = Examples(setting, s"$root.$rootA", s"$root.$rootB").examples

  override def widgets: Seq[(String, Option[WidgetInfo[Global]])] =
    Widgets(s"$root.$rootA", s"$root.$rootB").sortedWidgets
  end widgets

  override val footer: String =
    """Source code at: <a href="https://github.com/TelmoRibeiro/CoMPSeT">CoMPSeT source</a>.<br>
      |The tool builds upon: <a href="https:/github.com/arcalab/CAOS">CAOS</a>.<br>
      |It is concretely established over our extension of CAOS. available at: <a href="https:/github.com/TelmoRibeiro/CAOS">Extended CAOS source</a>.
      |""".stripMargin
end CaosConfigurator