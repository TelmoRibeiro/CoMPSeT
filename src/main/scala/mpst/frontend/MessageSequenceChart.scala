package mpst.frontend

import mpst.syntax.Protocol.*

object MessageSequenceChart:
  private def interaction2Mermaid(sender: Participant, receiver: Participant, label: Label): String =
    s"""$sender ->> $receiver :$label\n"""
  end interaction2Mermaid

  private def noteLeftOfAll(note: String)(using participants: Seq[Participant]): String =
    s"""note left of ${participants.head}: $note"""
  end noteLeftOfAll

  private def toMermaid(global: Global)(using participants: Seq[Participant]): String = global match
    case Interaction(sender, receiver, label, _) =>
      s"${interaction2Mermaid(sender, receiver, label)}"
    case Send(sender, receiver, label, _) =>
      s"${interaction2Mermaid(sender, receiver, label)}"
    case Receive(receiver, sender, label, _) =>
      s"${interaction2Mermaid(sender, receiver, label)}"
    case RecursionCall(variable) =>
      s"${noteLeftOfAll(s"goto $variable")}"
    case Sequence(globalA, globalB) =>
      s"""  ${toMermaid(globalA)}
         |  ${toMermaid(globalB)}""".stripMargin
    case Parallel(globalA, globalB) =>
      s"""par
         |  ${toMermaid(globalA)}
         |and
         |  ${toMermaid(globalB)}
         |end""".stripMargin
    case Choice(globalA, globalB) =>
      s"""alt
         |  ${toMermaid(globalA)}
         |else
         |  ${toMermaid(globalB)}
         |end""".stripMargin
    case RecursionFixedPoint(variable, globalB) =>
      s"""|rect rgb(50,200,200)
          |  ${noteLeftOfAll(s"Label $variable")}
          |  ${toMermaid(globalB)}
          |end""".stripMargin
    case RecursionKleeneStar(globalA) =>
      s"""loop
         |  ${toMermaid(globalA)}
         |end""".stripMargin
    case _ => ""
  end toMermaid

  def apply(global: Global): String =
    s"""
       |sequenceDiagram
       |${toMermaid(global)(using getParticipants(global).toSeq)}
       |""".stripMargin
  end apply
end MessageSequenceChart
