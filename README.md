----------
# Overview

This document describes how to run and recompile the tool:

- Using `compset.ova` (supports running/recompiling, *inside* a virtual machine)
- Using `compset-bin` (supports running, *outside* a virtual machine)
- Using `compset-src` (supports running/recompiling, *outside* a virtual machine)

--------------------
# How to use `compset.ova`

## Running the tool

1. Import `compset.ova` to the [VirtualBox](https://www.virtualbox.org/).

2. Login using the following credentials: "compset" (username) and "compset" (password).

3. Open `~/Desktop/compset-bin/index.html` in a web browser supporting `JavaScript`.

   left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

4. To visualize the global type as a sequence diagram, click on the `Message Sequence Chart` title (not the whole box; exactly the title).

5. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the reload button by the right side of the `Settings` title. When the button is hovered, a pop-up stating `Load settings` should appear.

   To visualize the projections as text, click on the `Locals` title.
   To visualize the projections as LTS, click on the `Local Automata` title.

6. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision.

   To visualize the LTS regarding the whole communication model, click on the `Local Compositional Automata` title.
   To reconstruct possible traces, click on the `Step-by-Step` title.

7. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision.

   Click on the `Bisimulation` title.

8. To suppress errors regarding the presence of recursion implementations, the matching option from `Recursion` must be selected. Confirm the decision.

9. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision.

10. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision.

11. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision.

12. New global types and settings can be experimented upon by tweaking with `Session` and `Settings` respectively (and confirming it).

    In this case, to remove errors provided by the previous sessions fix the issue and reload (through the reload button) the according widget (either `Settings` or `Session`).

## Recompiling the tool

1. [SBT](https://www.scala-sbt.org) and [JRE](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html) (Java Runtime Environment 1.8) are required.
    
   Both are already installed in the virtual machine.

2. Compile the code by opening a terminal in `compset-src` and executing:
   ```
   sbt fastOptJS
   ```
   
3. Open `~/Destktop/compset-src/lib/caos/tool/index.html` in a web browser supporting `JavaScript`.


-------------------------
# How to use `compset-bin`

## Running the tool

1. Open `compset-bin/index.html` in a web browser supporting `JavaScript`.

   left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

2. To visualize the global type as a sequence diagram, click on the `Message Sequence Chart` title (not the whole box; exactly the title).

3. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the reload button by the right side of the `Settings` title. When the button is hovered, a pop-up stating `Load settings` should appear.

   To visualize the projections as text, click on the `Locals` title.
   To visualize the projections as LTS, click on the `Local Automata` title.

4. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision.

   To visualize the LTS regarding the whole communication model, click on the `Local Compositional Automata` title.
   To reconstruct possible traces, click on the `Step-by-Step` title.

5. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision.

   Click on the `Bisimulation` title.

6. To suppress errors regarding the presence of recursion implementations, the matching option from `Recursion` must be selected. Confirm the decision.

7. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision.

8. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision.

9. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision.

10. New global types and settings can be experimented upon by tweaking with `Session` and `Settings` respectively (and confirming it).

    In this case, to remove errors provided by the previous sessions fix the issue and reload (through the reload button) the according widget (either `Settings` or `Session`).


-------------------------
# How to use `compset-src`

## Running the tool

1. Open `/compset-src/lib/caos/tool/index.html` in a web browser supporting `JavaScript`.

   left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

2. To visualize the global type as a sequence diagram, click on the `Message Sequence Chart` title (not the whole box; exactly the title).

3. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the reload button by the right side of the `Settings` title. When the button is hovered, a pop-up stating `Load settings` should appear.

   To visualize the projections as text, click on the `Locals` title.
   To visualize the projections as LTS, click on the `Local Automata` title.

4. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision.

   To visualize the LTS regarding the whole communication model, click on the `Local Compositional Automata` title.
   To reconstruct possible traces, click on the `Step-by-Step` title.

5. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision.

   Click on the `Bisimulation` title.

6. To suppress errors regarding the presence of recursion implementations, the matching option from `Recursion` must be selected. Confirm the decision.

7. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision.

8. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision.

9. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision.

10. New global types and settings can be experimented upon by tweaking with `Session` and `Settings` respectively (and confirming it).

    In this case, to remove errors provided by the previous sessions fix the issue and reload (through the reload button) the according widget (either `Settings` or `Session`).


## Recompiling the tool

1. Install [SBT](https://www.scala-sbt.org)

2. Install [JRE](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html) (Java Runtime Environment 1.8)

3. Compile the code by opening a terminal in `compset-src` and executing:
   ```
   sbt fastOptJS
   ```
   
4. Open `compset-src/lib/caos/tool/index.html` in a web browser supporting `JavaScript`.