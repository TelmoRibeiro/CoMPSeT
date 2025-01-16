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
- Fix MSC
- Fix Merge
- Propagate New Recursion
- Fix Semantic
- Add FullMerge
- Fix Sync
  - try with "a>b:Hello ; b>a:Bye"
- Fix Well-Formedness (Maybe Call It Projectability?)
- Fix Utilities
- Extend Example's List
___

## To Do (CAOS):
- remove unnecessary Option[Setting]  
- toggle leaf should toggle parents
- Uncheck Propagation Not Properly Working
- Conditional Widgets Do Not Properly Update When The Example Is Swapped Through Apply
- allowOne vs allowAll
- Do Not Break The Flow If The Conditions Fail
---

## Meeting:
- case Some / None as maps and getOrElse (options)