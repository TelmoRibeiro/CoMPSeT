package mpst.syntax

/* IDEA:
  - types used throughout the code
  - having all types here is not an elegant solution but Protocol.scala does not handle it well

  problem: check with prof. José Proença if I cannot move the first 2 sections to Protocol.scala
*/

object Type:
  // *global* & *local* types //
  type Global = Protocol
  type Local = Protocol
  // *protocol* related types //
  type Agent    = String
  type Message  = String
  type Sort     = String
  type Variable = String
  // *operational semantic* related types //
  type Action = Protocol
  type State  = (Map[String, Protocol], Protocol)
  type Queue  = Map[(Agent,Agent),List[Message]]
end Type