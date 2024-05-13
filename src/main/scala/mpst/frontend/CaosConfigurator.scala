package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.view.Text
import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

object CaosConfigurator extends Configurator[Protocol]:
  val name = "To Be Named..."
  override val languageName:String = "Protocol"
  val parser = mpst.syntax.Parser
  val examples = List(
    "MasterWorkers" -> "m>wA:Work<void> ; m>wB:Work<void> ; (wA>m:Done<void> || wB>m:Done<void>)",
  )
  val widgets = List(
    "view parsed protocol" -> view(_.toString,Text)
  )
end CaosConfigurator