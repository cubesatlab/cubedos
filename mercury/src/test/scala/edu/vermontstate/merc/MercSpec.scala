package edu.vermontstate.merc

class MercSpec extends UnitSpec {

  "Command line processing" should "behave as specified" in {
    val testCases: List[Array[String]] = List(
      Array("file.mxdr"),                    // Basic case, no switches.
      Array("-tfolder", "file.mxdr"),        // The -t switch.
      Array("-k", "file.mxdr"),              // The -k switch.
      Array("-k", "-tfolder", "file.mxdr"),  // Both switches.
      Array("-tfolder", "-k", "file.mxdr"),  // Both switches (other order).
      Array("-k"),                           // Missing file name.
      Array()                                // Empty command line.
    )

    val expectedResults: List[Option[(Map[Char, String], String)]] = List(
      Some((Map(), "file.mxdr")),
      Some((Map('t' -> "folder"), "file.mxdr")),
      Some((Map('k' -> ""), "file.mxdr")),
      Some((Map('k' -> "", 't' -> "folder"), "file.mxdr")),
      Some((Map('k' -> "", 't' -> "folder"), "file.mxdr")),
      None,
      None
    )

    for (i <- testCases.indices) {
      assert(
        Main.analyzeCommandLine(testCases(i)) == expectedResults(i),
        s"command line test with index $i failed")
    }
  }

}
