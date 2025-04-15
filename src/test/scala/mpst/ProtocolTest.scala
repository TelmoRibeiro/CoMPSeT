package mpst

import mpst.syntax.Protocol
import mpst.syntax.Protocol.*

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ProtocolTest extends Properties("Protocol"):
  property("exclusive locality") = forAll { (protocol: Protocol) =>
    ???
  }
end ProtocolTest