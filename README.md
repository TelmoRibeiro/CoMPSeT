# APIGenMPST
___

### In order to run:
**JVM and SBT must be installed**  
  
In **/APIGenMPST** run:
````bash
sbt clean
sbt compile
sbt run
````
___

### To Do:
- Actual Error Handling / PP
- Fix Use of Async vs Sync Interchangeably
- Taus? Oven and Choreo does it
- Fix AsyncProjection
- Add WSSemantic
- Merge Projectability Definitions (**Better Performance**)
- Develop Show (**pretty print**)
- Extend example's list
- Try to tail-recurse most functions
___

### Notes: 
- there is no mention of tail recursion in MSyncST
- MSyncST - projection erases parallel | Gentle - parallel is never defined
___

### Caos:
- new visual (and mandatory?) field for config
- config can be expressed by something as "[ Comm -> {AsyncMS,Sync}, Interleave -> {Off}, ...]"
- should config parser be supplied by the dev. like syntax parser?
- widgets gain an extra param named dependencies
- dependencies establish what elems from config are assumed (useful for Caos to sanity check if the widgets are respecting the original config)
- ex: "MSNet Semantics" -> steps(..., dependencies += {AsyncMS})
- a function should be supplied (by the dev.) in order to transform? verify? ...?