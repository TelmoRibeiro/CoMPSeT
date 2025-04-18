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

  private def mkSemantics: Setting =
    "Merge" -> ("Plain" || "Full") && "Comm Model" -> ("Sync" && "Async (Causal)" && "Async (Non-Causal)") && "Recursion" -> ("Kleene Star" || "Fixed Point") && "Parallel" && "Extra Requirements" -> ("Well Branched" && "Well Channeled")
  end mkSemantics

  override val setting: Setting = root -> mkSemantics

  override val examples: Seq[Example] = Examples(setting, root).examples

  override val widgets: Seq[(String, WidgetInfo[Global])] = Widgets(root).widgets
end CaosConfigurator