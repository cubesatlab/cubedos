package edu.vermontstate.merc

import java.io.File
import org.antlr.v4.runtime.*

class SyntaxSpec extends UnitSpec {

  "The MercParser" should "parse MXDR" in {
    val fileNames =
      Array("0001.mxdr", "0002.mxdr", "0003.mxdr", "0004.mxdr", "0005.mxdr",
            "0006.mxdr")

    for (fileName <- fileNames) {
      val fullName = "testData" + File.separator + "Syntax" + File.separator + fileName

      val codePointCharStream = CharStreams.fromFileName(fullName)
      val lexer  = new MXDRLexer(codePointCharStream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new MXDRParser(tokens)
      val tree   = parser.specification()  // 'tree' is intentionally ignored.
    }
  }
}
