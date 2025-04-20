package mpst.frontend.auxiliary

import caos.frontend.Configurator.Example
import caos.frontend.Setting

case class Examples(setting: Setting, root: String):
  private def mkVeryGentleIntroMPST: Setting = setting
    .setCheckedPath(s"$root.Merge.Full", true)
    .setCheckedPath(s"$root.Communication Model.Sync", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkVeryGentleIntroMPST

  private def mkGentleIntroMPAsyncST: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Async", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkGentleIntroMPAsyncST

  private def mkAPIGenInScala3: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Async", true)
    .setCheckedPath(s"$root.Parallel", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkAPIGenInScala3

  private def mkST4MP: Setting = setting
    .setCheckedPath(s"$root.Merge.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Async", true)
    .setCheckedPath(s"$root.Parallel", true)
    .setCheckedPath(s"$root.Recursion.Kleene Star", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkST4MP

  private val simpleDelegation: String = "pA->pB:TaskA || pA->pB:TaskB"

  private val simpleBranchingV1: String = "(pA->pB:TaskA ; pB->pC:TaskC)\n\t+\n(pA->pB:TaskB ; pB->pC:TaskC)"

  private val simpleBranchingV2: String = "(pA->pB:TaskA ; pB->pC:TaskA)\n\t+\n(pA->pB:TaskB ; pB->pC:TaskB)"

  private val controllerWorkerV0: String = "c->wA:Work ; c->wB:Work ;\nwA->c:Done ; wB->c:Done"

  private val controllerWorkerV1: String = "c->wA:Work ; c->wB:Work ;\n(wA->c:Done || wB->c:Done)"

  private val controllerWorkerV2: String = "(\n\tc->wA:Work ; c->wB:Work ;\n\t(wA->c:Done || wB->c:Done)\n)*"

  private val recursiveControllerWorker: String = "def X in \n\tc->w:Work ; w->c:Done ; X\n\t\t+\n\tc->w:Quit"

  private val badWellBranched: String = "(c->wA:Work ; wA->c:Done)\n\t+\n(c->wB:Work ; wB->c:Done)"

  private val badWellChannelled: String = "(c->w:TaskA ; w->c:Done)\n\t||\n(c->w:TaskB ; w->c:Done)"

  val examples: Seq[Example] = List(
    "controller-workers - v1"
      -> controllerWorkerV1
      -> "standard controller-workers (no settings)"
      -> setting,

    "controller-workers - v0"
      -> controllerWorkerV0
      -> "fully sequentialized controller-workers (no settings)"
      -> setting,

    "controller-workers - v2"
      -> controllerWorkerV2
      -> "standard controller-workers under kleene star recursion (no settings)"
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

    "controller-worker - fixed point recursion"
      -> recursiveControllerWorker
      -> "sequentialized controller-worker with fixed point recursion (no settings)"
      -> setting,

    "APIGenInScala3 settings"
      -> controllerWorkerV0
      -> "APIGenInScala3 settings (placeholder protocol)"
      -> mkAPIGenInScala3,

    "ST4MP settings"
      -> controllerWorkerV0
      -> "ST4MP settings (placeholder protocol)"
      -> mkST4MP,

    "VeryGentleIntroMPST settings"
      -> controllerWorkerV0
      -> "VeryGentleIntroMPST settings (placeholder protocol)"
      -> mkVeryGentleIntroMPST,

    "GentleIntroMPAsyncST settings"
      -> controllerWorkerV0
      -> "GentleIntroMPAsyncST settings (placeholder protocol)"
      -> mkGentleIntroMPAsyncST,

    "controller-workers - v1 (APIGenInScala3)"
      -> controllerWorkerV1
      -> "controller-workers-v1 under the APIGenInScala3 settings"
      -> mkAPIGenInScala3,

    "controller-workers - v2 (ST4MP)"
      -> controllerWorkerV2
      -> "controller-workers-v2 under the ST4MP settings"
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
      -> mkAPIGenInScala3.setCheckedPath(s"$root.Communication Model.Non-Causal Async", true),

    "controller-worker - fixed point recursion (ST4MP) | recursion fail"
      -> recursiveControllerWorker
      -> "failed recursion for the controller-worker - fixed point recursion under ST4MP settings"
      -> mkST4MP,

    "controller-workers - v2 (GentleIntroMPAsyncST) | parallel fail"
      -> controllerWorkerV2
      -> "failed parallel for the controller-workers - v2 under GentleIntroMPAsyncST settings"
      -> mkGentleIntroMPAsyncST,
  )
end Examples