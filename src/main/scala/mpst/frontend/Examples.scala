package mpst.frontend

import caos.frontend.Configurator.Example
import caos.frontend.Setting

case class Examples(setting: Setting, root: String):
  private def mkVeryGentleIntroMPST: Setting = setting
    .setCheckedPath(s"$root.Merge.Full", true)
    .setCheckedPath(s"$root.Comm Model.Sync", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkVeryGentleIntroMPST

  private def mkGentleIntroMPAsyncST: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkGentleIntroMPAsyncST

  private def mkAPIGenInScala3: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$root.Parallel", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkAPIGenInScala3

  private def mkST4MP: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Comm Model.Async (Causal)", true)
    .setCheckedPath(s"$root.Parallel", true)
    .setCheckedPath(s"$root.Recursion.Kleene Star", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkST4MP

  private val simpleDelegation: String = "m->w:TaskA || m->w:TaskB"

  private val simpleBranchingV1: String = "(wA->wB:TaskA ; wB->m:DoneA)\n\t+\n(wA->wB:TaskB ; wB->m:DoneA)"

  private val simpleBranchingV2: String = "(wA->wB:TaskA ; wB->m:DoneA)\n\t+\n(wA->wB:TaskB ; wB->m:DoneB)"

  private val mwv0: String = "m->wA:Work ; m->wB:Work ;\nwA->m:Done ; wB->m:Done"

  private val mwv1: String = "m->wA:Work ; m->wB:Work ;\n(wA->m:Done || wB->m:Done)"

  private val mwv2: String = "(\n\tm->wA:Work ; m->wB:Work ;\n\t(wA->m:Done || wB->m:Done)\n)*"

  private val recursiveMasterWorker: String = "def X in (\n\tm->w:Work ; w->m:Done ; X + m->w:Quit\n)"

  private val badWellBranched: String = "(m->wA:Work ; wA->m:Done)\n\t+\n(m->wB:Work ; wB->m:Done)"

  private val badWellChannelled: String = "(m->w:TaskA ; w->m:Done)\n\t||\n(m->w:TaskB ; w->m:Done)"

  val examples: Seq[Example] = List(
    "master-workers - v1"
      -> mwv1
      -> "standard master-workers (no settings)"
      -> setting,

    "master-workers - v0"
      -> mwv0
      -> "fully sequentialized master-workers (no settings)"
      -> setting,

    "master-workers - v2"
      -> mwv2
      -> "standard master-workers under kleene star recursion (no settings)"
      -> setting,

    "simple task delegation"
      -> simpleDelegation
      -> "a simple task delegation using parallel composition (no settings)"
      -> setting,

    "simple branching - v1"
      -> simpleBranchingV1
      -> "a simple branching protocol - plain-merge (no settings)"
      -> setting,

    "simple branching - v2"
      -> simpleBranchingV2
      -> "a simple branching protocol - full-merge (no settings)"
      -> setting,

    "master-worker - fixed point recursion"
      -> recursiveMasterWorker
      -> "sequentialized master-worker with fixed point recursion (no settings)"
      -> setting,

    "APIGenInScala3 settings"
      -> mwv0
      -> "APIGenInScala3 settings (placeholder protocol)"
      -> mkAPIGenInScala3,

    "ST4MP settings"
      -> mwv0
      -> "ST4MP settings (placeholder protocol)"
      -> mkST4MP,

    "VeryGentleIntroMPST settings"
      -> mwv0
      -> "VeryGentleIntroMPST settings (placeholder protocol)"
      -> mkVeryGentleIntroMPST,

    "GentleIntroMPAsyncST settings"
      -> mwv0
      -> "GentleIntroMPAsyncST settings (placeholder protocol)"
      -> mkGentleIntroMPAsyncST,

    "master-workers - v1 (APIGenInScala3)"
      -> mwv1
      -> "master-workers-v1 under the APIGenInScala3 settings"
      -> mkAPIGenInScala3,

    "master-workers - v2 (ST4MP)"
      -> mwv2
      -> "master-workers-v2 under the ST4MP settings"
      -> mkST4MP,

    "simple branching - v1 (GentleIntroMPAsyncST)"
      -> simpleBranchingV1
      -> "simple branching - v1 under the GentleIntroMPAsyncST"
      -> mkGentleIntroMPAsyncST,

    "simple branching - v2 (VeryGentleIntroMPST)"
      -> simpleBranchingV2
      -> "simple branching - v2 under the VeryGentleIntroMPST settings"
      -> mkVeryGentleIntroMPST,

    "simple task delegation (APIGenInScala3 vs Non-Causal Async.)"
      -> simpleDelegation
      -> "simple delegation under the APIGenInScala3 settings vs non-causal async. communication"
      -> mkAPIGenInScala3.setCheckedPath(s"$root.Comm Model.Async (Non-Causal)", true),

    "master-worker - fixed point recursion (ST4MP) | recursion fail"
      -> recursiveMasterWorker
      -> "failed recursion for the master-worker - fixed point recursion under ST4MP settings"
      -> mkST4MP,

    "master-workers - v2 (GentleIntroMPAsyncST) | parallel fail"
      -> mwv2
      -> "failed parallel for the master-workers - v2 under GentleIntroMPAsyncST settings"
      -> mkGentleIntroMPAsyncST,
  )
end Examples