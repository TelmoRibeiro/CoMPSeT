package mpst.syntax

import mpst.syntax.Protocol.*
import mpst.syntax.Type.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/* IDEA:
    just parse the input structure...

  @ telmo -
    identifier not caring about upper or lower case
*/

object Parser extends RegexParsers:
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  override def skipWhitespace: Boolean = true

  private def identifier: Parser[String] = """[a-zA-Z0-9_]+""".r

  private def structure: Parser[(Global,Set[Keyword])] =
    definition ~ opt("[" ~> repsep(keyword,",") <~ "]") ^^ {
      case globalType ~ configuration => (globalType,configuration.map(_.toSet).getOrElse(Set()))
    }
  end structure

  private def keyword:Parser[Keyword] =
    identifier ^^ {
      case "sync"         => Keyword.ComSync
      case "async-ms"     => Keyword.ComAsyncMS
      case "async-cs"     => Keyword.ComAsyncCS
      case "interleave-1" => Keyword.InterleaveOn
      case "interleave-0" => Keyword.InterleaveOff
      case "rec-k"        => Keyword.RecKleene
      case "rec-fp"       => Keyword.RecFixedPoint
      case "rec-0"        => Keyword.RecOff
    }

  private def definition:Parser[Global] =
    opt(globalType) ^^ (globalTypeSyntax => globalTypeSyntax.getOrElse(Protocol.Skip))
  end definition

  private def globalType: Parser[Protocol] =
    maybeParallel ~ opt(choice) ^^ {
      case maybeParallelSyntax ~ Some(choiceSyntax) => choiceSyntax(maybeParallelSyntax)
      case maybeParallelSyntax ~ None               => maybeParallelSyntax
    }
  end globalType

  private def choice: Parser[Protocol => Protocol] =
    "+" ~ maybeParallel ~ opt(choice) ^^ {
      case "+" ~ maybeParallelSyntax ~ Some(choiceSyntax) => (globalSyntax: Protocol) => choiceSyntax(Choice(globalSyntax, maybeParallelSyntax))
      case "+" ~ maybeParallelSyntax ~ None               => (globalSyntax: Protocol) => Choice(globalSyntax, maybeParallelSyntax)
      case _ ~ _ ~ _                                      => throw new RuntimeException("BAD SYNTAX")
    }
  end choice

  private def maybeParallel: Parser[Protocol] =
    maybeSequence ~ opt(parallel) ^^ {
      case maybeSequenceSyntax ~ Some(parallelSyntax) => parallelSyntax(maybeSequenceSyntax)
      case maybeSequenceSyntax ~ None                 => maybeSequenceSyntax
    }
  end maybeParallel

  private def parallel: Parser[Protocol => Protocol] =
    "||" ~ maybeSequence ~ opt(parallel) ^^ {
      case "||" ~ maybeSequenceSyntax ~ Some(parallelSyntax) => (globalSyntax: Protocol) => parallelSyntax(Parallel(globalSyntax, maybeSequenceSyntax))
      case "||" ~ maybeSequenceSyntax ~ None                 => (globalSyntax: Protocol) => Parallel(globalSyntax, maybeSequenceSyntax)
      case _ ~ _ ~ _                                         => throw new RuntimeException("BAD SYNTAX")
    }
  end parallel

  private def maybeSequence: Parser[Protocol] =
    atomGlobalType ~ opt(sequence) ^^ {
      case atomGlobalTypeSyntax ~ Some(sequenceSyntax) => sequenceSyntax(atomGlobalTypeSyntax)
      case atomGlobalTypeSyntax ~ None                 => atomGlobalTypeSyntax
    }
  end maybeSequence

  private def sequence: Parser[Protocol => Protocol] =
    ";" ~ atomGlobalType ~ opt(sequence) ^^ {
      case ";" ~ atomGlobalTypeSyntax ~ Some(sequenceSyntax) => (globalSyntax: Protocol) => sequenceSyntax(Sequence(globalSyntax, atomGlobalTypeSyntax))
      case ";" ~ atomGlobalTypeSyntax ~ None                 => (globalSyntax: Protocol) => Sequence(globalSyntax, atomGlobalTypeSyntax)
      case _ ~ _ ~ _                                         => throw new RuntimeException("BAD SYNTAX")
    }
  end sequence

  private def atomGlobalType: Parser[Protocol] = recursionFixedPoint | literal | recursionCall

  private def recursionFixedPoint: Parser[Protocol] =
    "def" ~ identifier ~ "in" ~ globalType ^^ {
      case "def" ~ recursionVariable ~ "in" ~ globalSyntax => RecursionFixedPoint(recursionVariable, globalSyntax)
      case _ ~ _ ~ _ ~ _                                  => throw new RuntimeException("BAD SYNTAX")
    }
  end recursionFixedPoint
  
  private def recursionCall: Parser[Protocol] = identifier ^^ (recursionVariable => RecursionCall(recursionVariable))

  private def literal: Parser[Protocol] = parentheses | message | end

  private def parentheses: Parser[Protocol] = "(" ~> globalType <~ ")"

  private def message: Parser[Protocol] =
    identifier ~ ">" ~ identifier ~ ":" ~ identifier ~ opt(sort) ^^ {
      case agentA ~ ">" ~ agentB ~ ":" ~ message ~ sort => Interaction(agentA,agentB,message,sort.getOrElse("void"))
    }
  end message

  private def sort:Parser[String] = "<" ~> identifier <~ ">"

  private def end:Parser[Protocol] = "end" ^^^ Skip

  def apply(input:String):Protocol =
    parseAll(globalType, input) match
      case Success(global,_) => global
      case failure:NoSuccess => throw new RuntimeException(s"parsing failed with msg=[${failure.msg}] and next=[${failure.next}]\n")
  end apply
end Parser