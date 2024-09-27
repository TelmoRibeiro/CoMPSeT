package mpst.syntax

/* IDEA:
    - keywords used in describing configurations...

   @ telmo -
    this follows an hacky first solution described by prof. José Proença
*/

@deprecated
enum Keyword:
  override def toString:String =
    this match
      case ComSync       => s"communication: synchronous"
      case ComAsyncMS    => s"communication: asynchronous | multiset"
      case ComAsyncCS    => s"communication: asynchronous | causal"
      case InterleaveOn  => s"interleaving: on"
      case InterleaveOff => s"interleaving: off"
      case RecKleene     => s"recursion: Kleene"
      case RecFixedPoint => s"recursion: Fixed Point"
      case RecOff        => s"recursion: off"
  end toString

  case ComSync
  case ComAsyncMS
  case ComAsyncCS
  case InterleaveOn
  case InterleaveOff
  case RecKleene
  case RecFixedPoint
  case RecOff
end Keyword