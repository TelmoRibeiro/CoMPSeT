package mpst.syntax

import mpst.utilities.Multiset

/* IDEA:
    types used throughout the code
    having all types here is not an elegant solution but Protocol.scala does not handle it well

  @ telmo -
    check with prof. José Proença if I cannot move the first 2 sections to Protocol.scala
*/

object Type:
  // *global* & *local* types //
  type Global = Protocol
  type Local  = Protocol
  // *protocol* related types //
  type Agent    = String
  type Message  = String
  type Sort     = String
  type Variable = String
  // *operational semantic* related types //
  type Action = Protocol
  type State  = (Map[String, Protocol], Protocol)
  type Queue  = Map[(Agent,Agent),List[Message]]
  // *configuration* related types //
  type Configuration = (Global,Set[Keyword])
  // wrappers //
  type StateWrapper = (Protocol,Map[Variable,Protocol])

  type LocalEnv = Map[Agent, Map[Variable, Local]]
  type NetStateWrapper = (Set[(Agent, Local)], Multiset[Action], LocalEnv)
end Type