package mpst.syntax

import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/* IDEA:
    just parse the input structure...

  @ telmo - NOT REVIEWED
    identifier not caring about upper or lower case
*/

object Parser extends RegexParsers:
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  override def skipWhitespace: Boolean = true

  private def identifier: Parser[String] = """[a-zA-Z0-9_]+""".r

  private def session:Parser[Global] =
    opt(globalType) ^^ (globalTypeSyntax => globalTypeSyntax.getOrElse(Protocol.Skip))
  end session

  private def globalType:Parser[Global] =
    maybeParallel ~ opt(choice) ^^ {
      case maybeParallelSyntax ~ Some(choiceSyntax) => choiceSyntax(maybeParallelSyntax)
      case maybeParallelSyntax ~ None               => maybeParallelSyntax
    }
  end globalType

  private def choice:Parser[Global=>Global] =
    "+" ~ maybeParallel ~ opt(choice) ^^ {
      case "+" ~ maybeParallelSyntax ~ Some(choiceSyntax) => (globalSyntax:Global) => choiceSyntax(Protocol.Choice(globalSyntax,maybeParallelSyntax))
      case "+" ~ maybeParallelSyntax ~ None               => (globalSyntax:Global) => Protocol.Choice(globalSyntax,maybeParallelSyntax)
      case _ ~ _ ~ _                                      => throw new RuntimeException("BAD SYNTAX")
    }
  end choice

  private def maybeParallel:Parser[Global] =
    maybeSequence ~ opt(parallel) ^^ {
      case maybeSequenceSyntax ~ Some(parallelSyntax) => parallelSyntax(maybeSequenceSyntax)
      case maybeSequenceSyntax ~ None                 => maybeSequenceSyntax
    }
  end maybeParallel

  private def parallel:Parser[Global=>Global] =
    "||" ~ maybeSequence ~ opt(parallel) ^^ {
      case "||" ~ maybeSequenceSyntax ~ Some(parallelSyntax) => (globalSyntax:Global) => parallelSyntax(Protocol.Parallel(globalSyntax,maybeSequenceSyntax))
      case "||" ~ maybeSequenceSyntax ~ None                 => (globalSyntax:Global) => Protocol.Parallel(globalSyntax,maybeSequenceSyntax)
      case _ ~ _ ~ _                                         => throw new RuntimeException("BAD SYNTAX")
    }
  end parallel

  private def maybeSequence:Parser[Global] =
    atomGlobalType ~ opt(sequence) ^^ {
      case atomGlobalTypeSyntax ~ Some(sequenceSyntax) => sequenceSyntax(atomGlobalTypeSyntax)
      case atomGlobalTypeSyntax ~ None                 => atomGlobalTypeSyntax
    }
  end maybeSequence

  private def sequence:Parser[Global=>Global] =
    ";" ~ atomGlobalType ~ opt(sequence) ^^ {
      case ";" ~ atomGlobalTypeSyntax ~ Some(sequenceSyntax) => (globalSyntax:Global) => sequenceSyntax(Protocol.Sequence(globalSyntax,atomGlobalTypeSyntax))
      case ";" ~ atomGlobalTypeSyntax ~ None                 => (globalSyntax:Global) => Protocol.Sequence(globalSyntax,atomGlobalTypeSyntax)
      case _ ~ _ ~ _                                         => throw new RuntimeException("BAD SYNTAX")
    }
  end sequence

  private def atomGlobalType:Parser[Global] = recursionFixedPoint | literal | recursionCall

  private def recursionFixedPoint:Parser[Global] =
    "def" ~ identifier ~ "in" ~ globalType ^^ {
      case "def" ~ recursionVariable ~ "in" ~ globalSyntax => Protocol.RecursionFixedPoint(recursionVariable,globalSyntax)
      case _ ~ _ ~ _ ~ _                                   => throw new RuntimeException("BAD SYNTAX")
    }
  end recursionFixedPoint
  
  private def recursionCall:Parser[Global] = identifier ^^ (recursionVariable => Protocol.RecursionCall(recursionVariable))

  private def literal:Parser[Global] = parentheses | interaction | skip

  private def parentheses:Parser[Global] = "(" ~> globalType <~ ")"

  private def interaction:Parser[Global] =
    identifier ~ ">" ~ identifier ~ ":" ~ identifier ~ opt(sort) ^^ {
      case agentA ~ ">" ~ agentB ~ ":" ~ message ~ sort => Protocol.Interaction(agentA,agentB,message,sort.getOrElse("void"))
      case _ ~ _ ~ _ ~ _ ~ _ ~ _                        => throw new RuntimeException("BAD SYNTAX")
    }
  end interaction

  private def sort:Parser[String] = "<" ~> identifier <~ ">"

  private def skip:Parser[Global] = "skip" ^^^ Protocol.Skip

  def apply(input:String):Global =
    parseAll(session,input) match
      case Success(global,_) => global
      case failure:NoSuccess => throw new RuntimeException(s"parsing failed with msg=[${failure.msg}] and next=[${failure.next}]\n")
  end apply
end Parser