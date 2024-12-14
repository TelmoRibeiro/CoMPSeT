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
- Add Update On setSetting
- Conditional Widgets Do Not Properly Update When The Example Is Swapped Through Apply
- allowOne vs allowAll
- Evade The setChecked Issue
- Render On WidgetInfo vs On String -> WidgetInfo
- Do Not Break The Flow If The Conditions Fail