package mpst.frontend.auxiliary.view

import mpst.syntax.Protocol.*

object MessageSequenceChart:
  private def noteOverAll(note: String)(using participants: Seq[Participant]): String =
    s"""note over ${participants.mkString(",")}: $note"""
  end noteOverAll

  private def interaction2Mermaid(sender: Participant, receiver: Participant, label: Label): String =
    s"""$sender ->> $receiver :$label\n"""
  end interaction2Mermaid

  private def toMermaid(global: Global)(using participants: Seq[Participant]): String = global match
    case interaction: Interaction =>
      s"${interaction2Mermaid(interaction.sender, interaction.receiver, interaction.label)}"
    case sendAction: Send =>
      s"${interaction2Mermaid(sendAction.sender, sendAction.receiver, sendAction.label)}"
    case recvAction: Recv =>
      s"${interaction2Mermaid(recvAction.sender, recvAction.receiver, recvAction.label)}"
    case RecursionCall(variable) =>
      s"${noteOverAll(s"goto $variable")}"
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
      s"""|rect rgb(213,256,251)
          |  ${noteOverAll(s"Label $variable")}
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