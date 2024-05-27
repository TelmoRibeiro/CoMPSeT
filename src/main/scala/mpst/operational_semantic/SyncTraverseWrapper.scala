package mpst.operational_semantic

import caos.sos.SOS

import mpst.syntax.Type.*

import mpst.operational_semantic.SyncTraverse

import mpst.utilities.Multiset

/* IDEA:

  @ telmo -
*/

object SyncTraverseWrapper extends SOS[Action,SynStateWrapper]:
  override def accepting(state:SynStateWrapper):Boolean =
    val locals -> _ = state
      SyncTraverse.accepting(locals)
  end accepting

  override def next[A>:Action](state:SynStateWrapper):Set[(A,SynStateWrapper)] =
    val locals -> localEnv = state
    val nextAction -> nextLocals = SyncTraverse.next(locals)(using localEnv)
    val result = nextAction -> (nextLocals -> localEnv)
    Set() + result
  end next
end SyncTraverseWrapper