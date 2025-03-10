package mpst.syntax

import mpst.syntax.Protocol.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


object Parser extends RegexParsers:
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  override def skipWhitespace: Boolean = true

  private def identifier: Parser[String] = """[a-zA-Z0-9_]+""".r

  private def session: Parser[Global] = opt(globalType) ^^ (globalTypeSyntax => globalTypeSyntax.getOrElse(Skip))

  private def globalType: Parser[Global] = maybeInterleaving ~ opt(choice) ^^ {
    case maybeParallelSyntax ~ Some(choiceSyntax) => choiceSyntax(maybeParallelSyntax)
    case maybeParallelSyntax ~ None               => maybeParallelSyntax
  }
  end globalType

  private def choice: Parser[Global => Global] = "+" ~ maybeInterleaving ~ opt(choice) ^^ {
    case "+" ~ maybeParallelSyntax ~ Some(choiceSyntax) => (globalSyntax: Global) => choiceSyntax(Choice(globalSyntax, maybeParallelSyntax))
    case "+" ~ maybeParallelSyntax ~ None               => (globalSyntax: Global) => Choice(globalSyntax, maybeParallelSyntax)
    case _ ~ _ ~ _  => throw RuntimeException("bad syntax on choice")
  }
  end choice

  private def maybeInterleaving: Parser[Global] = maybeSequence ~ opt(interleaving) ^^ {
    case maybeSequenceSyntax ~ Some(parallelSyntax) => parallelSyntax(maybeSequenceSyntax)
    case maybeSequenceSyntax ~ None                 => maybeSequenceSyntax
  }
  end maybeInterleaving

  private def interleaving: Parser[Global => Global] = "||" ~ maybeSequence ~ opt(interleaving) ^^ {
    case "||" ~ maybeSequenceSyntax ~ Some(parallelSyntax) => (globalSyntax: Global) => parallelSyntax(Parallel(globalSyntax, maybeSequenceSyntax))
    case "||" ~ maybeSequenceSyntax ~ None                 => (globalSyntax: Global) => Parallel(globalSyntax, maybeSequenceSyntax)
    case _ ~ _ ~ _ => throw RuntimeException("bad syntax on parallel")
  }
  end interleaving

  private def maybeSequence: Parser[Global] = atomGlobalType ~ opt(sequence) ^^ {
    case atomGlobalTypeSyntax ~ Some(sequenceSyntax) => sequenceSyntax(atomGlobalTypeSyntax)
    case atomGlobalTypeSyntax ~ None                 => atomGlobalTypeSyntax
  }
  end maybeSequence

  private def sequence: Parser[Global => Global] = ";" ~ atomGlobalType ~ opt(sequence) ^^ {
    case ";" ~ atomGlobalTypeSyntax ~ Some(sequenceSyntax) => (globalSyntax: Global) => sequenceSyntax(Sequence(globalSyntax ,atomGlobalTypeSyntax))
    case ";" ~ atomGlobalTypeSyntax ~ None                 => (globalSyntax: Global) => Sequence(globalSyntax, atomGlobalTypeSyntax)
    case _ ~ _ ~ _ => throw RuntimeException("bad syntax on sequence")
  }
  end sequence

  private def atomGlobalType: Parser[Global] = recursionFixedPoint | literal | recursionCall

  private def recursionFixedPoint: Parser[Global] = "def" ~ identifier ~ "in" ~ globalType ^^ {
    case "def" ~ recursionVariable ~ "in" ~ globalSyntax => RecursionFixedPoint(recursionVariable, globalSyntax)
    case _ ~ _ ~ _ ~ _ => throw RuntimeException("bad syntax on recursionFixedPoint")
  }
  end recursionFixedPoint

  private def recursionCall: Parser[Global] = identifier ^^ (recursionVariable => RecursionCall(recursionVariable))

  private def literal: Parser[Global] = recursionKleeneStar | parentheses | interaction | skip


  private def recursionKleeneStar: Parser[Global] = parentheses ~ "*" ^^ {
    case globalType ~ "*" => RecursionKleeneStar(globalType)
    case _ ~ _ => throw RuntimeException("bad syntax on recursionKleeneStar")  
  }

  private def parentheses: Parser[Global] = "(" ~> globalType <~ ")"

  private def interaction: Parser[Global] = identifier ~ "->" ~ identifier ~ ":" ~ identifier ^^ {
    case sender ~ "->" ~ receiver ~ ":" ~ label => Interaction(sender, receiver, label)
    case _ ~ _ ~ _ ~ _ ~ _ => throw RuntimeException("bad syntax on interaction")
  }
  end interaction

  private def skip: Parser[Global] = "skip" ^^^ Skip

  def apply(input: String): Global = parseAll(session, input) match
    case Success(global, _) => global
    case failure: NoSuccess => throw new RuntimeException(s"parsing failed with msg=[${failure.msg}] and next=[${failure.next}]\n")
  end apply
end Parser