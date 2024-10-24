package mpst.syntax

import mpst.utilities.Multiset

import scala.collection.immutable.Queue

/* @ telmo
  IDEA:
    => Collection of all the internal types used throughout the CoMPSeT project.
  ISSUES:
    => None
  REVIEWED:
    => AFFIRMATIVE* => if I manage to simplify somewhere else, I should come back here
*/

object Type:
  // *global* & *local* types //
  type Global = Protocol
  type Local  = Protocol
  // *protocol* related types //
  type Participant = String
  type Label       = String
  type Sort        = String
  type Variable    = String
  // *operational semantic* related types //
  type Action = Protocol
  type State  = (Map[String, Protocol], Protocol)
  type ChannelQueue = Map[(Participant, Participant), Queue[Label]]
  // wrappers //
  type StateWrapper = (Protocol, Map[Variable, Protocol])
  // environment //
  type Environment = Map[Participant, Map[Variable, Local]]
end Type