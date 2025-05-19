package mpst.frontend.auxiliary

import caos.frontend.Configurator.Example
import caos.frontend.Setting

case class Examples(setting: Setting, rootA: String, rootB: String):
  private def mkVeryGentleIntroMPST(root: String): Setting = setting
    .setCheckedPath(s"$root.Merge Criteria.Full", true)
    .setCheckedPath(s"$root.Communication Model.Synchronous", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkVeryGentleIntroMPST

  private def mkGentleIntroMPAsyncST(root: String): Setting = setting
    .setCheckedPath(s"$root.Merge Criteria.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Asynchronous", true)
    .setCheckedPath(s"$root.Recursion.Fixed Point", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
  end mkGentleIntroMPAsyncST

  private def mkAPIGenInScala3(root: String): Setting = setting
    .setCheckedPath(s"$root.Merge Criteria.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Asynchronous", true)
    .setCheckedPath(s"$root.Parallel Composition", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkAPIGenInScala3

  private def mkST4MP(root: String): Setting = setting
    .setCheckedPath(s"$root.Merge Criteria.Plain", true)
    .setCheckedPath(s"$root.Communication Model.Causal Asynchronous", true)
    .setCheckedPath(s"$root.Parallel Composition", true)
    .setCheckedPath(s"$root.Recursion.Kleene Star", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Branched", true)
    .setCheckedPath(s"$root.Extra Requirements.Well Channeled", true)
  end mkST4MP

  private val simpleDelegation: String = "pA->pB:TaskA || pA->pB:TaskB"

  private val simpleBranchingV1: String = "(pA->pB:TaskA ; pB->pC:TaskC)\n\t+\n(pA->pB:TaskB ; pB->pC:TaskC)"

  private val simpleBranchingV2: String = "(pA->pB:TaskA ; pB->pC:TaskA)\n\t+\n(pA->pB:TaskB ; pB->pC:TaskB)"

  private val controllerWorkerV0: String = "c->wA:Work ; c->wB:Work ;\nwA->c:Done ; wB->c:Done"

  private val controllerWorkerV1: String = "c->wA:Work ; c->wB:Work ;\n(wA->c:Done || wB->c:Done)"

  private val recursiveControllerWorkerV1: String = "(c->w:Work ; w->c:Done)*"

  private val recursiveControllerWorkerV2: String = "def X in \n\tc->w:Work ; w->c:Done ; X\n\t\t+\n\tc->w:Quit"

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

    "recursive controller-worker - v1"
      -> recursiveControllerWorkerV1
      -> "controller-worker with kleene star recursion (no settings)"
      -> setting,

    "recursive controller-worker - v2"
      -> recursiveControllerWorkerV2
      -> "controller-worker with fixed point recursion (no settings)"
      -> setting,

    "simple branching - v1"
      -> simpleBranchingV1
      -> "a simple branching protocol - plain-merge (no settings)"
      -> setting,

    "simple branching - v2"
      -> simpleBranchingV2
      -> "a simple branching protocol - full-merge (no settings)"
      -> setting,

    "simple task delegation"
      -> simpleDelegation
      -> "a simple task delegation using parallel composition (no settings)"
      -> setting,

    "controller-workers - v1 (APIGenInScala3)"
      -> controllerWorkerV1
      -> "controller-workers-v1 under the APIGenInScala3 settings"
      -> mkAPIGenInScala3(rootA),

    "recursive controller-worker - v1 (ST4MP)"
      -> recursiveControllerWorkerV1
      -> "recursive controller-worker - v1 under the ST4MP settings"
      -> mkST4MP(rootA),

    "simple branching - v1 (GentleIntroMPAsyncST)"
      -> simpleBranchingV1
      -> "simple branching - v1 under the GentleIntroMPAsyncST"
      -> mkGentleIntroMPAsyncST(rootA),

    "simple branching - v2 (VeryGentleIntroMPST)"
      -> simpleBranchingV2
      -> "simple branching - v2 under the VeryGentleIntroMPST settings"
      -> mkVeryGentleIntroMPST(rootA),
  )
end Examples