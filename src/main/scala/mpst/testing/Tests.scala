package mpst.testing

import mpst.encoding.NoEncoding
import mpst.projection.AsyncProjection
import mpst.syntax.Parser
import mpst.syntax.Protocol

@deprecated
object Tests:
  // Standard Testing //
  private val testList: List[(String, List[String])] =
    List (
      ("STANDARD LIST",
      List (
        // Expected Output: accepted //
        "m>wA:Work<void> ; m>wB:Work<void> ; (wA>m:Done<void> || wB>m:Done<void>)",
      )),
      ("FREE VARIABLES LIST",
        List (  
          // Expected Output: rejected - Y   belongs to FVs //
          "def X in (m>wA:Work<void> || m>wB:Done<void>) ; Y",
          // Expected Output: rejected - X   belongs to FVs //
          "(m>wA:Work<void> || m>wB:Work<void>) ; X",
          // Expected Output: rejected - X_2 belongs to FVs //
          "def X in (m>wA:Work<void> || m>wB:Work<void>) ; X ; X",
        )),
      ("RACE CONDITIONS",
        List (
          // Expected Output: rejected - possible race condition //
          "def X in (m>wA:Work<void> ; X  || m>wB:Work<void> ; X)",
          // Expected Output: accepted //
          "def X in (m>wA:Work<void> || m>wB:Work<void>) ; X",
          // Expected Output: accepted //
          "(def X in m>wA:Work<void> ; X) || (def Y in m>wB:Work<void> ; Y)",
        )),
      ("DISAMBIGUATION",
        List (
          // Expected Output: rejected - "buyer>seller:Msg" in both branches //
          "(broker>buyer:Notify<void> ; buyer>seller:Msg<void> ; seller>buyer:Pay<void> + broker>buyer:Quit<void> ; buyer>seller:Msg<void>)",
          // Expected Output: accepted //
          "(broker>buyer:Notify<void> ; buyer>seller:Price<void> ; seller>buyer:Pay<void> + broker>buyer:Quit<void> ; buyer>seller:Stop<void>)",
        )),
      ("LINEARITY",
        List (
          // Expected Output: rejected - "wB>m:None" in both branches //
          "m>wA:Work<void> ; m>wB:Work<void> ; (wA>m:Done<void> ; wB>m:None<void> || wA>m:None<void> ; wB>m:None<void>)",
        )),
      ("SELF COMMUNICATION",
        List(
          // Expected Output: rejected - no self communication //
          "m>wA:Work<void> ; wA>wA:Done<void>"
        )),
    )
  end testList
  
  private def test(protocolList: List[String]): Unit =
    for protocol <- protocolList yield
      println(s"PROTOCOL: $protocol")
      val global: Protocol = Parser(protocol)
      println(s"GLOBAL TYPE: $global")
      // should run analysis here!
      for agent -> local <- AsyncProjection.projectionWithAgent(global) yield
        println(s"LOCAL TYPE ($agent): $local")
        // NoEncoding(local)
        println()
      println()
      println()
  end test

  def apply(): Unit =
    println("SOME HACKY TESTS:")
    for (_, testCases) <- testList yield
      test(testCases)
  end apply
end Tests