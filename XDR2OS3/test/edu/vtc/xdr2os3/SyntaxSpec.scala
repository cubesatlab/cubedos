package edu.vtc.xdr2os3

import java.io.File
import org.antlr.v4.runtime._

class SyntaxSpec extends UnitSpec {

  "The syntax of the test files" should "be correct" in {
    val fileNames = Array("0001.mxdr")

    for (fileName <- fileNames) {
      val fullName = "testData" + File.separator + "Syntax" + File.separator + fileName

      val input = new ANTLRFileStream(fullName)
      val lexer  = new XDRLexer(input)
      val tokens = new CommonTokenStream(lexer)
      val parser = new XDRParser(tokens)
      val tree   = parser.specification()
    }
  }
}
