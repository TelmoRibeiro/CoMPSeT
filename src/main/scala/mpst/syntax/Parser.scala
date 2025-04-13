package mpst.syntax

import mpst.syntax.Protocol.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


object Parser extends RegexParsers:
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  override def skipWhitespace: Boolean =
    true
  end skipWhitespace

  private val reservedKeywords: Set[String] = Set("skip", "def", "in")

  private def identifier: Parser[String] =
    """[\w_]+""".r.withFilter(!reservedKeywords.contains(_))
  end identifier

  private def session: Parser[Global] =
    opt(globalType) ^^ {
      globalTypeOption => globalTypeOption.getOrElse(Skip)
    }
  end session

  private def globalType: Parser[Global] = maybeParallel ~ opt(choice) ^^ {
    case maybeParallel ~ Some(choice) => choice(maybeParallel)
    case maybeParallel ~ None         => maybeParallel
  }
  end globalType

  private def choice: Parser[Global => Global] = "+" ~ maybeParallel ~ opt(choice) ^^ {
    case "+" ~ maybeParallel ~ Some(choice) => (global: Global) => choice(Choice(global, maybeParallel))
    case "+" ~ maybeParallel ~ None         => (global: Global) => Choice(global, maybeParallel)
    case _ ~ _ ~ _  => throw RuntimeException("bad syntax on choice")
  }
  end choice

  private def maybeParallel: Parser[Global] = maybeSequence ~ opt(parallel) ^^ {
    case maybeSequence ~ Some(parallel) => parallel(maybeSequence)
    case maybeSequence ~ None           => maybeSequence
  }
  end maybeParallel

  private def parallel: Parser[Global => Global] = "||" ~ maybeSequence ~ opt(parallel) ^^ {
    case "||" ~ maybeSequence ~ Some(parallel) => (global: Global) => parallel(Parallel(global, maybeSequence))
    case "||" ~ maybeSequence ~ None           => (global: Global) => Parallel(global, maybeSequence)
    case _ ~ _ ~ _ => throw RuntimeException("bad syntax on parallel")
  }
  end parallel

  private def maybeSequence: Parser[Global] = atomGlobalType ~ opt(sequence) ^^ {
    case atomGlobalType ~ Some(sequence) => sequence(atomGlobalType)
    case atomGlobalType ~ None           => atomGlobalType
  }
  end maybeSequence

  private def sequence: Parser[Global => Global] = ";" ~ atomGlobalType ~ opt(sequence) ^^ {
    case ";" ~ atomGlobalType ~ Some(sequence) => (global: Global) => sequence(Sequence(global, atomGlobalType))
    case ";" ~ atomGlobalType ~ None           => (global: Global) => Sequence(global, atomGlobalType)
    case _ ~ _ ~ _ => throw RuntimeException("bad syntax on sequence")
  }
  end sequence

  private def atomGlobalType: Parser[Global] =
    recursionKleeneStar | recursionFixedPoint | literal
  end atomGlobalType

  private def recursionKleeneStar: Parser[Global] = parentheses ~ "*" ^^ {
    case globalType ~ "*" => RecursionKleeneStar(globalType)
    case _ ~ _ => throw RuntimeException("bad syntax on recursionKleeneStar")
  }
  end recursionKleeneStar

  private def recursionFixedPoint: Parser[Global] = "def" ~ identifier ~ "in" ~ globalType ^^ {
    case "def" ~ recursionVariable ~ "in" ~ global => RecursionFixedPoint(recursionVariable, global)
    case _ ~ _ ~ _ ~ _ => throw RuntimeException("bad syntax on recursionFixedPoint")
  }
  end recursionFixedPoint

  private def literal: Parser[Global] =
    parentheses | skip | interaction | recursionCall
  end literal

  private def parentheses: Parser[Global] =
    "(" ~> globalType <~ ")"
  end parentheses

  private def skip: Parser[Global] =
    "skip" ^^^ Skip
  end skip

  private def interaction: Parser[Global] = identifier ~ "->" ~ identifier ~ ":" ~ identifier ^^ {
    case sender ~ "->" ~ receiver ~ ":" ~ label => Interaction(sender, receiver, label)
    case _ ~ _ ~ _ ~ _ ~ _ => throw RuntimeException("bad syntax on interaction")
  }
  end interaction

  private def recursionCall: Parser[Global] = identifier ^^ {
    recursionVariable => RecursionCall(recursionVariable)
  }
  end recursionCall

  def apply(input: String): Global = parseAll(session, input) match
    case Success(global, _) => global
    case failure: NoSuccess => throw new RuntimeException(s"parsing failed with msg=[${failure.msg}] and next=[${failure.next}]\n")
  end apply
end Parser