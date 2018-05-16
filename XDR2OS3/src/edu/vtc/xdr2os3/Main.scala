package edu.vtc.xdr2os3

import java.io.PrintStream
import org.antlr.v4.runtime._


/**
 * The main module of the program. It parses the command line, reads the input XDR file, and
 * coordinates the work of the rest of the application.
 */
object Main {

  /**
    * Analyze the command line and extract the switches as well as the name of the source
    * file(s) to be compiled. Each switch is has a '-' as a prefix and is represented as a
    * single letter. If the switch has an argument, the text of that argument immediately
    * follows the switch itself (no space) and extends to the next non-quoted space on the
    * command line.
    *
    * @param args The command line as passed to the main method.
    * @return None if there is an error in the command line; otherwise return a set of switches
    *         together with the source file name(s).
    */
  def analyzeCommandLine(args: Array[String]): Option[(Map[Char, String], String)] = {
    if (args.length < 1) {
      None
    }
    else if (args.length == 1) {
      Some(Map(), args(0))
    }
    else {
      val (switchArgs, nonSwitchArgs) = args partition { _.charAt(0) == '-' }

      // Extract the switches.
      // Invalid switches are transformed into the special '-' switch, which isn't used.
      val switchMap: Map[Char, String] = (for (switch: String <- switchArgs.toSet) yield {
        // TODO: Deal with the 'empty' switch consisting of just a single '-'
        val rawSwitch   = switch.charAt(1)
        val rawArgument = if (switch.length == 2) "" else switch.substring(2)
        // TODO: Issue an error message for invalid switches.
        rawSwitch -> rawArgument
      }).toMap

      // For now we support only a single source file name. If no names or multiple names are
      // found, set the name to a blank string, which isn't used.
      // TODO: Add support for multiple source files in a single run?
      val sourceName = if (nonSwitchArgs.length == 1) nonSwitchArgs(0) else ""

      // Decide what to return.
      // TODO: Return None if an invalid switch is found.
      if (sourceName == "") None else Some(switchMap, sourceName)
    }
  }


  def main(args: Array[String]): Unit = {
    // Analyze the command line.
    val commandOption = analyzeCommandLine(args)
    if (commandOption.isEmpty) {
      println("Usage: java -jar xdr2os3.jar [-k] [-tTemplateFolder] mxdr-source-file")
      System.exit(1)
    }
    val Some((switchMap, sourceName)) = commandOption
    val templateFolder = switchMap.getOrElse('t', ".")

    // Create a stream that reads from the specified file.
    val input = new ANTLRFileStream(sourceName)

    // Parse the input file as XDR.
    val lexer  = new XDRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new XDRParser(tokens)
    val tree   = parser.specification()

    // Walk the tree created during the parse and analyze it for semantic errors.
    val symbolTable = new BasicSymbolTable
    val reporter    = new BasicConsoleReporter
    val myTable     = new STPopulation(sourceName, symbolTable, reporter)
    myTable.visit(tree)
    val myAnalyzer  = new SemanticAnalyzer(sourceName, symbolTable, reporter)
    myAnalyzer.visit(tree)

    val errorCount = reporter.errorCount

    if (switchMap.contains('k')) {
      println(s"*** $errorCount errors found.")
    }
    else if (errorCount > 0) {
      println(s"*** $errorCount errors found. Aborting!")
    }
    else {
      println(s"$sourceName Successfully parsed and analyzed...")
      println("Generating API package...")

      // TODO: This won't work for paths without dots. Should that be ruled out earlier?
      val baseFileName = sourceName.substring(0, sourceName.lastIndexOf('.'))
      val baseFileNameLower = baseFileName.toLowerCase
      val specificationFile = new PrintStream("cubedos-" + baseFileNameLower + "-api.ads")
      val bodyFile = new PrintStream("cubedos-" + baseFileNameLower + "-api.adb")

      val mySpecificationGenerator =
        new SpecificationGenerator(templateFolder, baseFileName, symbolTable, specificationFile, reporter)
      mySpecificationGenerator.visit(tree)

      val myBodyGenerator =
        new BodyGenerator(templateFolder, baseFileName, symbolTable, bodyFile, reporter)
      myBodyGenerator.visit(tree)

      specificationFile.close()
      bodyFile.close()
    }
  }

}
