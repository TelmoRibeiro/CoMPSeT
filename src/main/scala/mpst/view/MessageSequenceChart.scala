// package mpst.view

// import mpst.syntax.Protocol.*
// import mpst.syntax.Type.*

/*
  IDEA:
    picking cool visualizations from prof. José Proença
*/

/*
object MessageSequenceChart:

  private def toMermaid(global:Global):String =
    global match
      case Interaction(agentA,agentB,message,sort) =>
        s"""rect rgb(238,238,238)
           | $agentA > $agentB : $message
           | end
           |""".stripMargin
      case RecursionCall(variable) =>
        s"""toDo"""
      case Skip =>
        s"""toDo"""
      case Sequence(globalA,globalB) =>
        s""" ${toMermaid(globalA)}
           | ${toMermaid(globalB)}""".stripMargin
      case Parallel(globalA,globalB) =>
        s"""par
           | ${toMermaid(globalA)}
           | and
           | ${toMermaid(globalB)}
           | end""".stripMargin
      case Choice(globalA,globalB) =>
        s"""alt
           | ${toMermaid(globalA)}
           | else
           | ${toMermaid(globalB)}
           | end""".stripMargin
      case RecursionFixedPoint(variable,globalB) =>
        s"""toDo"""
      case local => throw new RuntimeException()
  end toMermaid

  def apply(global:Global):String =
    s"""
       |sequenceDiagram
       |${toMermaid(global)}
       |""".stripMargin
  end apply
end MessageSequenceChart
*/