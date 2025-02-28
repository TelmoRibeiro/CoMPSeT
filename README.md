----------
# Overview

This document describes how to run and recompile the tool:

- Using `CoMPSeT.ova` (supports running/recompiling, *inside* a virtual machine)
- Using `compset-bin` (supports running, *outside* a virtual machine)
- Using `compset-src` (supports running/recompiling, *outside* a virtual machine)

--------------------
# How to use `CoMPSeT.ova`

## Running the tool

1. Import `CoMPSeT.ova` in [VirtualBox](https://www.virtualbox.org/).

2. Login using the following credentials: "compsetvm" (username) and "compsetvm" (password).

3. Open `~/Desktop/compset-src/lib/caos/tool/index.html` in a web browser supporting `JavaScript`.

   left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

4. To visualize the global type as a sequence diagram, click on the `Message Sequence Diagram` title (not the whole box; exactly the title).

5. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title. When the button is hovered, a pop out stating `Load settings` should appear.

   To visualize the projections as text, click on the `Locals` title (exactly the title).
   To visualize the projections as lts, click on the `Local Automata` title (exactly the title).

6. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

   To visualize the lts regarding the whole communication model, click on the `Local Compositional Automata` title (exactly).
   To reconstruct possible traces, click on the `Step-by-Step` title (exactly).

7. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

   Click on the `Bisimulation` title (exactly).

8. To suppress errors regarding the presence of recursion implementation, an option from `Recursion` must be selected. Confirm the decision by clicking on the button on right side of the `Settings` title.

9. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

10. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

11. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

12. To remove errors provided by previous sessions reload both `Settings` and `Session`, after the issue is fixed.

13. New global types and settings can be experimented upon by tweaking with the `Session` and `Settings` field (and confirming it).

## Recompiling the tool

1. [SBT](https://www.scala-sbt.org) and [JRE](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html) (Java Runtime Environment 1.8) are required.
    Both are already installed in the virtual machine.
2. Compile the code by opening a terminal in `compset-src` and executing:
   ```
   sbt fastOptJS
   ```


-------------------------
# How to use `compset-bin`

## Running the tool

1. Open `compset-bin/index.html` in a web browser supporting `JavaScript`.

   The left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

2. To visualize the global type as a sequence diagram, click on the `Message Sequence Diagram` title (not the whole box; exactly the title).

3. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title. When the button is hovered, a pop out stating `Load settings` should appear.

   To visualize the projections as text, click on the `Locals` title (exactly the title).
   To visualize the projections as lts, click on the `Local Automata` title (exactly the title).

4. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

   To visualize the lts regarding the whole communication model, click on the `Local Compositional Automata` title (exactly).
   To reconstruct possible traces, click on the `Step-by-Step` title (exactly).

5. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

   Click on the `Bisimulation` title (exactly).

6. To suppress errors regarding the presence of recursion implementation, an option from `Recursion` must be selected. Confirm the decision by clicking on the button on right side of the `Settings` title.

7. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

8. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

9. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

10. To remove errors provided by previous sessions reload both `Settings` and `Session`, after the issue is fixed.

11. New global types and settings can be experimented upon by tweaking with the `Session` and `Settings` field (and confirming it).


-------------------------
# How to use `compset-src`

## Running the tool

1. Open `compset-src/lib/caos/tool/index.html` in a web browser supporting `JavaScript`.

    The left panels provide the input interface. It consists of an input form to write new global types (`Session`), checkboxes to modify semantical properties (`Settings`), and buttons to load existing examples (`Examples`). By default, the `master-workers - v1` example is loaded. The right panels provide the output interface.

2. To visualize the global type as a sequence diagram, click on the `Message Sequence Diagram` title (not the whole box; exactly the title).

3. To visualize the projections, an option from `Merge` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title. When the button is hovered, a pop out stating `Load settings` should appear.
    
    To visualize the projections as text, click on the `Locals` title (exactly the title).
    To visualize the projections as lts, click on the `Local Automata` title (exactly the title).

4. To interact with the communication model, an option from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

    To visualize the lts regarding the whole communication model, click on the `Local Compositional Automata` title (exactly).
    To reconstruct possible traces, click on the `Step-by-Step` title (exactly).

5. To visualize the `Bisimulation` comparison, multiple options from `Comm Model` must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

    Click on the `Bisimulation` title (exactly).

6. To suppress errors regarding the presence of recursion implementation, an option from `Recursion` must be selected. Confirm the decision by clicking on the button on right side of the `Settings` title.

7. To suppress errors regarding the presence of parallel composition, the `Parallel` option must be selected. Confirm the decision by clicking on the button on the right side of the `Settings` title.

8. To check if the branching behaviour complies with the expected behaviour from MPST, the `Well Branched` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

9. To make the parallel composition comply with different communication on different sub-protocols, the `Well Channeled` option must be selected under `Extra Requirements`. Confirm the decision by clicking on the button on the right side of the `Settings` title.

10. To remove errors provided by previous sessions reload both `Settings` and `Session`, after the issue is fixed.  

11. New global types and settings can be experimented upon by tweaking with the `Session` and `Settings` field (and confirming it).


## Recompiling the tool

1. Install [SBT](https://www.scala-sbt.org)
2. Install [JRE](https://www.oracle.com/java/technologies/javase/javase8-archive-downloads.html) (Java Runtime Environment 1.8)
3. Compile the code by opening a terminal in `compset-src` and executing:
   ```
   sbt fastOptJS
   ```