package mpst.frontend

import mpst.syntax.Protocol.Global

import caos.frontend.Site.initSite

/* @ telmo
  IDEA:
    => initialise CAOS.Site
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE
*/

object Main:
  def main(args:Array[String]):Unit = initSite[Global](CaosConfigurator)
end Main