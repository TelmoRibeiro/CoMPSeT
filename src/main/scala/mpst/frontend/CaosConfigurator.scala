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

  private val root = "Semantics"
  private val rootA = "Semantics A"
  private val rootB = "Semantics B"

  private def mkSemantics: Setting =
    "Merge" -> ("Plain" || "Full") && "Communication Model" -> ("Sync" || "Causal Async" || "Non-Causal Async") && "Recursion" -> ("Kleene Star" || "Fixed Point") && "Parallel" && "Extra Requirements" -> ("Well Branched" && "Well Channeled")
  end mkSemantics

  override val setting: Setting = Setting(root, List(s"$root.$rootA" -> mkSemantics, s"$root.$rootB" -> mkSemantics), options = List("allowAll"))

  override val examples: Seq[Example] = Examples(setting, s"$root.$rootA", s"$root.$rootB").examples

  override val widgets: Seq[(String, WidgetInfo[Global])] = Widgets(s"$root.$rootA", s"$root.$rootB").widgets
end CaosConfigurator