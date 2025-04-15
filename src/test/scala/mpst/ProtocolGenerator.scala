package mpst

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Gen.*


object ProtocolGenerator:
  private val identifier: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  given Arbitrary[Protocol] = Arbitrary {
    def genProtocol(depth: Int): Gen[Protocol] = {
      val base: List[Gen[Protocol]] = List(
        for
          sender <- identifier
          receiver <- identifier
          label <- identifier
        yield Interaction(sender, receiver, label),
        for
          sender <- identifier
          receiver <- identifier
          label <- identifier
        yield Send(sender, receiver, label),
        for
          sender <- identifier
          receiver <- identifier
          label <- identifier
        yield Recv(receiver, sender, label),
        for
          variable <- identifier
        yield RecursionCall(variable),
        const(Skip)
      )

      val recursive: List[Gen[Protocol]] = {
        if depth <= 0 then base else base ++ List(
          for
            protocolA <- genProtocol(depth - 1)
            protocolB <- genProtocol(depth - 1)
          yield Sequence(protocolA, protocolB),
          for
            protocolA <- genProtocol(depth - 1)
            protocolB <- genProtocol(depth - 1)
          yield Parallel(protocolA, protocolB),
          for
            protocolA <- genProtocol(depth - 1)
            protocolB <- genProtocol(depth - 1)
          yield Choice(protocolA, protocolB),
          for
            variable <- identifier
            protocolB <- genProtocol(depth - 1)
          yield RecursionFixedPoint(variable, protocolB),
          for
            protocolA <- genProtocol(depth - 1)
          yield RecursionKleeneStar(protocolA)
        )
      }
    }
    sized(n => genProtocol(n % 4 + 1))
  }
end ProtocolGenerator