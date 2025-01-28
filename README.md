# CoMPSeT
___

## To Run:
**JVM and SBT must be installed**  
  
In **/CoMPSeT** run:
````bash
sbt clean
sbt fastOptJS
````
through **index.html** you can visualize the **CAOS** interface
___

## To Do (Core):
- review "Sync"
- review "Full Merge"
- extra requirements should be on caos (selectable)
- klenne star (check if well propagated)
- missing StructuralCongruence() (check plain merge)
- MSC (add fixed point recursion)
- global semantic (revisit the sequence problem)
- well-formedness (revisit it - change it for projectability)
- extended examples
___

## To Do (CAOS):
- implicit renaming on the new examples (with widgets)
- parenthesis do not always work (ex: (a && b) && c)
---

## Meeting:
- case Some / None as maps and getOrElse (options)