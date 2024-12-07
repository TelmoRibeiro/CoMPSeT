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
- Add 2nd Recursion
- Fix Semantic
- Do Not Break The Flow If The Conditions Fail
- Add FullMerge
- Fix Sync
  - try with "a>b:Hello ; b>a:Bye"
- Fix Well-Formedness (Maybe Call It Projectability?)
- Fix Utilities
- Extend Example's List
___

## To Do (CAOS):

- Evade The setChecked Issue
- try to move Configurator.str2setting and Configurator.toSettingRenamed
- add allowOne vs allowAll - different UI behaviour
- experiment with exclusive orders
- swap null for Option[Setting]
- Render On WidgetInfo vs On String -> WidgetInfo
- There is some problems with the right bar after switching examples without applying configs
  - I believe this problem is related to the way I am using check but need to further inspect