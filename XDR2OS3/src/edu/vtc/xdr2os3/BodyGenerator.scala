package edu.vtc.xdr2os3

import java.io.{File, PrintWriter}

import edu.vtc.xdr2os3.XDRParser._
import org.antlr.v4.runtime.tree._

class BodyGenerator(
  templateFolder : String,
  nameOfFile     : String,
  symbolTable    : BasicSymbolTable,
  out            : java.io.PrintStream,
  reporter       : Reporter) extends XDRBaseVisitor[Void] {

  // The number of indentations where output lines start.
  private var indentationLevel = 0
  private var typeOfModule = ""

  private def doIndentation() {
    for (i <- 0 until indentationLevel) {
      out.print("   ")
    }
  }

  private def insertLine(bulk: List[String], str: String, indicator: String): List[String] = {
    var newBulk = List[String]()
    for (line <- bulk) {
      newBulk = newBulk.::(line)
      if (line.contains(indicator)) {
        newBulk = newBulk.::("\n" + str)
      }
    }
    newBulk.reverse
  }

  def addedLines(lines: List[String]): List[String] = {
    var flagTimer = 0
    for (i <- symbolTable.getMStructs) {
      for (ii <- symbolTable.getSType(i)) {
        if (symbolTable.getST(i, ii).contains("TimeSpanRep") ||
          symbolTable.getST(i, ii).contains("TimeRep")) {
          flagTimer = 1
        }
      }
    }
    var l = List[String]()
    if (flagTimer == 1) {
      val s1 = "with Ada.Real_Time;\nuse Ada.Real_Time;"
      val a1 = "SPARK_Mode(On);"
      val finalTemplateLines = insertLine(lines, s1, a1)
      l = finalTemplateLines
    }
    else {
      l = lines
    }
    l
  }

  def processTemplate(): List[String] = {
    val source = scala.io.Source.fromFile("/home/CubeSat/Projects/CubedOS/XDR2OS3/mxdrTests/template2.adb")
    val lines = source.getLines().toList
    val newLines = addedLines(lines)
    source.close()
    newLines
  }

  override def visitSpecification(ctx: XDRParser.SpecificationContext): Void = {
    val lines = processTemplate()
    val replacementString = nameOfFile
    for (line <- lines) {
      val newLine = line.replaceAllLiterally("%MODULENAME%", replacementString)
      if (line.contains("%BULK%")) {
        indentationLevel += 1
        visitChildren(ctx)
        indentationLevel -= 1
      }
      else {
        out.println(newLine)
      }
    }
    null
  }

  //Process appropriate variable names for struct elements before decode body.
  def processStructDF(i: String, l: List[String], structNum: Int): Void = {
    if (structNum == 0) {
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        var sd = ""
        for (u <- l) {
          if (u != "") {
            sd = sd.concat(u)
            sd = sd.concat("_")
          }
        }
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructDF(sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (symbolTable.getArraySType(i, y) == "DataRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")

          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
          }
          else {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + " : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "_Size : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println("Raw_Size : XDR.XDR_Unsigned;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Boolean;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Boolean;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("Raw_Interval  : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("Seconds : Ada.Real_Time.Seconds_Count;")
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
      }
    }
    else {
      var sd = ""
      for (u <- l) {
        if (u != "") {
          if (u == l.head) {
            sd = sd.concat(u + "_")
          }
          else {
            sd = sd.concat(u)
            if (u != l.last) {
              sd = sd.concat("_")
            }
          }
        }
      }
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructDF(sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (symbolTable.getArraySType(i, y) == "DataRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
          }
          else {
            doIndentation()
            out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + " : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "_Size : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println("Raw_Size : XDR.XDR_Unsigned;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Boolean;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Boolean;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Hyper;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Double;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Float;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Integer;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("Raw_Interval  : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("Seconds : Ada.Real_Time.Seconds_Count;")
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
        else {
          doIndentation()
          out.println("Raw_" + sd + y + "   : XDR.XDR_Unsigned;")
        }
      }
    }
    null
  }

  //Process struct elements appropriately in decodes.
  def processStructD(id: String, i: String, l: List[String], posFlag: Int, stringFlag: String, structNum: Int): Void = {
    if (structNum == 0) {
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        var sd = ""
        for (u <- l) {
          if (u != "") {
            sd = sd.concat(u)
            sd = sd.concat("_")
          }
        }
        var sf = ""
        for (u <- l) {
          if (u != "") {
            sf = sf.concat(u)
            if (u != l.last) {
              sf = sf.concat(".")
            }
          }
        }
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructD(i, sp, ll, posFlag, stringFlag, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in " + sp +
              "'Pos(" + sp + "'First) .. " +
              sp + "'Pos(" + sp +
              "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "'Val(Raw_" + sd + y + ");")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + "_Size, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y +
              "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(y + "_Size := Natural(Raw_" + sd + y + "_Size);")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "_Size := 0;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            doIndentation()
            out.println("if " + sd + y + "_Size < 1 then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, " + sd + y + "(" + sd + y + "'First .. " + y +
              "'First + (" + sd + y + "_Size - 1)), Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println(sd + "." + y + "(I)" + " := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println(sd + "." + y + "(I)" + " := " + sp + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type" +
              "'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.U_Hyper_Type(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(Lib.Hyper_Type" +
              "'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.Hyper_Type(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(" + sp +
              "'First) .. XDR.XDR_Unsigned_Hyper(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(" + sp +
              "'First) .. XDR.XDR_Hyper(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Double(Double" +
              "'First) .. XDR.XDR_Double(Double'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Double(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Double(" + sp +
              "'First) .. XDR.XDR_Double(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Float(Float" +
              "'First) .. XDR.XDR_Float(Float'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Float(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Float(" + sp +
              "'First) .. XDR.XDR_Float(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.Quadruple_Octet(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Integer(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in " + sp +
            "'Pos(" + sp + "'First) .. " +
            sp + "'Pos(" + sp +
            "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "'Val(Raw_" + sd + y + ");")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + "_Size, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + sd + y +
            "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(y + "_Size := Natural(Raw_" + sd + y + "_Size);")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println(sd + y + "_Size := 0;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          doIndentation()
          out.println("if " + sd + y + "_Size < 1 then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, " + y + "(" + y + "'First .. " + y +
            "'First + (" + y + "_Size - 1)), Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println(i + " := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println(i + " := " + sp + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.U_Hyper_Type(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(Lib.Hyper_Type'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.Hyper_Type(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(" + sp +
            "'First) .. XDR.XDR_Unsigned_Hyper(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(" + sp +
            "'First) .. XDR.XDR_Hyper(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Double(Double'First) .. XDR.XDR_Double(Double'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Double(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Double(" + sp +
            "'First) .. XDR.XDR_Double(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Float(Float'First) .. XDR.XDR_Float(Float'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Float(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Float(" + sp +
            "'First) .. XDR.XDR_Float(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.Quadruple_Octet(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Integer(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(" + sp +
            "'First) .. XDR.XDR_Unsigned(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(" + sp +
            "'First) .. XDR.XDR_Integer(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep") || symbolTable.getST(i, y).contains("OpaqueRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_Size, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println("Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          doIndentation()
          out.println("if Size < " + y + "'Length then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, " + y + "(" + y + "'First .. " + y + "'First + Size - 1), Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_Interval, Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_Interval < XDR.XDR_Unsigned(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(y + " := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + y + " < XDR.XDR_Unsigned(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println("Seconds := Ada.Real_Time.Seconds_Count(Raw_" + y + ");")
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(" + sp +
            "'First) .. XDR.XDR_Unsigned(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
      }
    }
    else {
      var sd = ""
      for (u <- l) {
        if (u != "") {
          if (u == l.head) {
            sd = sd.concat(u + "_")
          }
          else {
            sd = sd.concat(u)
            if (u != l.last) {
              sd = sd.concat("_")
            }
          }
        }
      }
      var sf = ""
      for (u <- l) {
        if (u != "") {
          if (u == l.head) {
            sf = sf.concat(u + "(Y)")
          }
          else {
            sf = sf.concat(u)
            if (u != l.last) {
              sf = sf.concat(".")
            }
          }
        }
      }

      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructD(i, sp, ll, posFlag, stringFlag, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in " + sp +
              "'Pos(" + sp + "'First) .. " +
              sp + "'Pos(" + sp +
              "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "'Val(Raw_" + sd + y + ");")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + "_Size, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y +
              "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(y + "_Size := Natural(Raw_" + sd + y + "_Size);")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "_Size := 0;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            doIndentation()
            out.println("if " + sd + y + "_Size < 1 then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, " + sd + y + "(" + sd + y + "'First .. " + y +
              "'First + (" + sd + y + "_Size - 1)), Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println(sd + "." + y + "(I)" + " := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println(sd + "." + y + "(I)" + " := " + sp + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type" +
              "'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.U_Hyper_Type(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(Lib.Hyper_Type" +
              "'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.Hyper_Type(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(" + sp +
              "'First) .. XDR.XDR_Unsigned_Hyper(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(" + sp +
              "'First) .. XDR.XDR_Hyper(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Double(Double" +
              "'First) .. XDR.XDR_Double(Double'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Double(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Double(" + sp +
              "'First) .. XDR.XDR_Double(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Float(Float" +
              "'First) .. XDR.XDR_Float(Float'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Float(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Float(" + sp +
              "'First) .. XDR.XDR_Float(" + sp + "'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Lib.Quadruple_Octet(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := Integer(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("if Decode_Status = Success then")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
            indentationLevel += 1
            doIndentation()
            out.println(sf + "." + y + "(I)" + " := " + sp + "(Raw_" + sd + y + ");")
            doIndentation()
            out.println("Decode_Status := Success;")
            indentationLevel -= 1
            doIndentation()
            out.println("else")
            indentationLevel += 1
            doIndentation()
            out.println("Decode_Status := Malformed;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end if;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in " + sp +
            "'Pos(" + sp + "'First) .. " +
            sp + "'Pos(" + sp +
            "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "'Val(Raw_" + sd + y + ");")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + "_Size, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + sd + y +
            "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(y + "_Size := Natural(Raw_" + sd + y + "_Size);")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println(sd + y + "_Size := 0;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          doIndentation()
          out.println("if " + sd + y + "_Size < 1 then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, " + y + "(" + y + "'First .. " + y +
            "'First + (" + y + "_Size - 1)), Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println(i + " := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println(i + " := " + sp + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.U_Hyper_Type(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(Lib.Hyper_Type'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.Hyper_Type(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned_Hyper(" + sp +
            "'First) .. XDR.XDR_Unsigned_Hyper(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Hyper(" + sp +
            "'First) .. XDR.XDR_Hyper(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Double(Double'First) .. XDR.XDR_Double(Double'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Double(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Double(" + sp +
            "'First) .. XDR.XDR_Double(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Float(Float'First) .. XDR.XDR_Float(Float'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Float(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Float(" + sp +
            "'First) .. XDR.XDR_Float(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Lib.Quadruple_Octet(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := Integer(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(" + sp +
            "'First) .. XDR.XDR_Unsigned(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Integer(" + sp +
            "'First) .. XDR.XDR_Integer(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep") || symbolTable.getST(i, y).contains("OpaqueRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_Size, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println("Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          doIndentation()
          out.println("if Size < " + y + "'Length then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, " + y + "(" + y + "'First .. " + y + "'First + Size - 1), Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_Interval, Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_Interval < XDR.XDR_Unsigned(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(y + " := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + y + " < XDR.XDR_Unsigned(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println("Seconds := Ada.Real_Time.Seconds_Count(Raw_" + y + ");")
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
        else {
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + sd + y + ", Last);")
          if (posFlag == 1 && y == symbolTable.getSType(i).last && sd == stringFlag) {
            //do Nothing!
          }
          else {
            doIndentation()
            out.println("Position := Last + 1;")
          }
          doIndentation()
          out.println("if Raw_" + sd + y + " in XDR.XDR_Unsigned(" + sp +
            "'First) .. XDR.XDR_Unsigned(" + sp + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(sf + "." + y + " := " + sp + "(Raw_" + sd + y + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
        }
      }
    }
    null
  }

  //Process same variable names for encodes as decodes based on struct elements.
  def processStructE(id: String, i: String, l: List[String], structNum: Int): Void = {
    if (structNum == 0) {
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        var sf = ""
        for (u <- l) {
          if (u != "") {
            sf = sf.concat(u)
            if (u != l.last) {
              sf = sf.concat(".")
            }
          }
        }
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructE(i, sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sp + "'Pos(" + sf + "." + y + "(I))), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "(I)'Length), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("XDR.Encode(" + sf + "." + y + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Double(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Float(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Hyper(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Boolean(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Integer(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + sp + "'Pos(" + sf + "." + y + ")), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + id + "." + y + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + id + "." + y + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + id + "." + y + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + id + "." + y + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Integer(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Double(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Float(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Hyper(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Boolean(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
      }
    }
    else {
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        var sf = ""
        for (u <- l) {
          if (u != "") {
            if (u == l.head) {
              sf = sf.concat(u + "(Y)")
            }
            else {
              sf = sf.concat(u)
              if (u != l.last) {
                sf = sf.concat(".")
              }
            }
          }
        }
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructE(i, sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "EnumRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sp + "'Pos(" + sf + "." + y + "(I))), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "(I)'Length), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("XDR.Encode(" + sf + "." + y + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Double(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Float(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Hyper(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Boolean(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Integer(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("EnumRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + sp + "'Pos(" + sf + "." + y + ")), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + id + "." + y + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + id + "." + y + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + id + "." + y + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + id + "." + y + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Integer(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Double(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Float(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Hyper(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Boolean(" + sf + "." + y + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
      }
    }
    null
  }

  //Process struct element variable initializations.
  def processStructV(i: String, l: List[String], structNum: Int): Void = {
    if (structNum == 0) {
      var sd = ""
      for (u <- l) {
        if (u != "") {
          sd = sd.concat(u)
          sd = sd.concat(".")
        }
      }
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructV(sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := (others => ' ');")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Boolean'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.Hyper(XDR.XDR_Hyper'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Double'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Float'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Integer(XDR.XDR_Integer'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println(sd + y + " := (others => ' ');")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println(sd + y + " := Boolean'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.Hyper(XDR.XDR_Hyper'First);")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println(sd + y + " := Double'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println(sd + y + " := Float'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println(sd + y + " := Integer(XDR.XDR_Integer'First);")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println(sd + y + " := (others => 0);")
          doIndentation()
          out.println("Size := 0;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_Span_First;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_First;")
        }
        else if (symbolTable.getST(i, y).contains("VoidRep")) {
          //do nothing!
        }
        else {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
      }
    }
    else {
      var sd = ""
      for (u <- l) {
        if (u != "") {
          if (u == l.head) {
            sd = sd.concat(u + "(Y).")
          }
          else {
            sd = sd.concat(u)
            if (u != l.last) {
              sd = sd.concat(".")
            }
          }
        }
      }
      for (y <- symbolTable.getSType(i)) {
        val sp = symbolTable.getStructuredTypeParent(i, y)
        if (symbolTable.getST(i, y).contains("StructRep")) {
          var ll = List[String]()
          ll = l.:+(y)
          processStructV(sp, ll, 0)
        }
        else if (symbolTable.getST(i, y).contains("ArrayRep")) {
          if (symbolTable.getArraySType(i, y) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := (others => ' ');")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Boolean'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.Hyper(XDR.XDR_Hyper'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Double'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Float'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp == "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := Integer(XDR.XDR_Integer'First);")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (sp != "null" && symbolTable.getArraySType(i, y) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(i, y) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println(sd + y + "(I) := " + sp + "'First;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(i, y).contains("StringRep")) {
          doIndentation()
          out.println(sd + y + " := (others => ' ');")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println(sd + y + " := Boolean'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("BoolRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.Hyper(XDR.XDR_Hyper'First);")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UHyperRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("HyperRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println(sd + y + " := Double'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("DoubleRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println(sd + y + " := Float'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("FloatRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println(sd + y + " := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
        }
        else if (sp == "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println(sd + y + " := Integer(XDR.XDR_Integer'First);")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("UIntRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (sp != "null" && symbolTable.getST(i, y).contains("IntRep")) {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
        else if (symbolTable.getST(i, y).contains("DataRep")) {
          doIndentation()
          out.println(sd + y + " := (others => 0);")
          doIndentation()
          out.println("Size := 0;")
        }
        else if (symbolTable.getST(i, y).contains("TimeSpanRep")) {
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_Span_First;")
        }
        else if (symbolTable.getST(i, y).contains("TimeRep")) {
          doIndentation()
          out.println(y + " := Ada.Real_Time.Time_First;")
        }
        else if (symbolTable.getST(i, y).contains("VoidRep")) {
          //do nothing!
        }
        else {
          doIndentation()
          out.println(sd + y + " := " + sp + "'First;")
        }
      }
    }
    null
  }

  //Encode sending.
  def doEncode(ctx: XDRParser.Struct_bodyContext, id: String, arrowFlag: Int): Void = {
    val encodeString = "_Encode"
    doIndentation()
    out.println("function " + id + encodeString)
    indentationLevel += 1
    doIndentation()
    if (arrowFlag == 0) {
      out.println("(Sender_Domain : Domain_ID_Type;")
      doIndentation()
      out.println("Sender : Module_ID_Type;")
      doIndentation()
      out.println("Request_ID : Request_ID_Type;")
    }
    else if (arrowFlag == 1) {
      out.println("(Receiver_Domain : Domain_ID_Type;")
      doIndentation()
      out.println("Receiver : Module_ID_Type;")
      doIndentation()
      out.println("Request_ID : Request_ID_Type;")
    }
    val structStuff = ctx.declaration().size()
    for (i <- 0 until structStuff) {
      if (ctx.declaration(i).children.contains(ctx.declaration(i).VOID())) {
        doIndentation()
        out.println("--TODO")
      }
      else {
        val t = ctx.declaration(i).children.get(0).getText
        val idd = ctx.declaration(i).IDENTIFIER().getText
        doIndentation()
        if (t == "opaque") {
          out.println(idd + " : CubedOS.Lib.Octet_Array;")
        }
        else if (t == "int") {
          out.println(idd + " : Integer;")
        }
        else if (t == "unsignedhyper") {
          out.println(id + " : Lib.U_Hyper_Type;")
        }
        else if (t == "unsignedint") {
          out.println(id + " : Lib.Quadruple_Octet;")
        }
        else if (t == "double") {
          out.println(idd + " : Double;")
        }
        else if (t == "float") {
          out.println(idd + " : Float;")
        }
        else if (t == "hyper") {
          out.println(idd + " : Lib.Hyper_Type;")
        }
        else if (t == "bool") {
          out.println(idd + " : Boolean;")
        }
        else if (t == "string") {
          out.println(idd + " : String;")
        }
        else {
          out.println(idd + " : " + ctx.declaration(i).type_specifier().getText + ";")
        }
      }
    }
    doIndentation()
    out.println("Priority : System.Priority := System.Default_Priority) return Message_Record")
    indentationLevel -= 1
    doIndentation()
    out.println("is")
    indentationLevel += 1
    if (arrowFlag == 0) {
      doIndentation()
      out.println("Message : Message_Record;")
    }
    else if (arrowFlag == 1) {
      doIndentation()
      out.println("Message : Message_Record := Make_Empty_Message(")
      indentationLevel += 1
      doIndentation()
      out.println("Sender_Domain   => Domain_ID,")
      doIndentation()
      out.println("Receiver_Domain => Receiver_Domain,")
      doIndentation()
      out.println("Sender     => ID,")
      doIndentation()
      out.println("Receiver   => Receiver,")
      doIndentation()
      out.println("Request_ID   => Request_ID,")
      doIndentation()
      out.println("Message_ID => Message_Type'Pos(" + id + "),")
      doIndentation()
      out.println("Priority   => Priority);")
      indentationLevel -= 1
    }
    doIndentation()
    out.println("Position : XDR_Index_Type;")
    if (ctx.declaration(0).getText != "void") {
      doIndentation()
      out.println("Last : XDR_Index_Type;")
    }
    for (i <- symbolTable.getSType(id)) {
      if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
        doIndentation()
        out.println("Interval : constant Duration := Ada.Real_Time.To_Duration(" + i + ");")
      }
      else if (symbolTable.getST(id, i).contains("TimeRep")) {
        doIndentation()
        out.println("Seconds  : Ada.Real_Time.Seconds_Count;")
        doIndentation()
        out.println("Fraction : Ada.Real_Time.Time_Span;")
      }
    }
    indentationLevel -= 1
    doIndentation()
    out.println("begin")
    indentationLevel += 1
    if (arrowFlag == 0) {
      doIndentation()
      out.println("Message := Make_Empty_Message(")
      indentationLevel += 1
      doIndentation()
      out.println("Sender_Domain   => Sender_Domain,")
      doIndentation()
      out.println("Receiver_Domain => Domain_ID,")
      doIndentation()
      out.println("Sender     => Sender,")
      doIndentation()
      out.println("Receiver   => ID,")
      doIndentation()
      out.println("Request_ID   => Request_ID,")
      doIndentation()
      out.println("Message_ID => Message_Type'Pos(" + id + "),")
      doIndentation()
      out.println("Priority   => Priority);")
      indentationLevel -= 1
    }
    else if (arrowFlag == 1) {
      //Do nothing!
    }
    for (i <- symbolTable.getSType(id)) {
      if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
        doIndentation()
        out.println("")
      }
      else if (symbolTable.getST(id, i).contains("TimeRep")) {
        doIndentation()
        out.println("Ada.Real_Time.Split(" + i + ", Seconds, Fraction);")
        out.println("")
      }
    }
    doIndentation()
    out.println("Position := 0;")
    for (i <- symbolTable.getSType(id)) {
      if (symbolTable.getST(id, i).contains("VoidRep")) {
        doIndentation()
        out.println("--TODO")
      }
      else {
        var s = symbolTable.getStructuredTypeParent(id, i)
        if (symbolTable.getST(id, i).contains("StructRep")) {
          var l = List[String]()
          l = l.:+(i)
          processStructE(id, s, l, 0)
        }
        else if (symbolTable.getST(id, i).contains("ArrayRep")) {
          if (symbolTable.getArraySType(id, i) == "StructRep") {
            var l = List[String]()
            l = l.:+(i)
            val structNum = symbolTable.getArraySSize(id, i).toInt
            doIndentation()
            out.println("for Y in Integer range 0 .. " + symbolTable.getArraySSize(id, i).toInt + " loop")
            indentationLevel += 1
            processStructE(id, s, l, structNum)
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "EnumRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + s + "'Pos(" + i + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "StringRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + i + "(I)'Length), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            doIndentation()
            out.println("XDR.Encode(" + i + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "UHyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + i + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "HyperRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Hyper(" + i + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getStructuredTypeParent(id, i) == "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Boolean'Val(Boolean'Pos(" + i + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getStructuredTypeParent(id, i) != "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Boolean'Val(" + symbolTable.getStructuredTypeParent(id, i) + "'Pos(" + i + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "UIntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Unsigned(" + i + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "IntRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Integer(" + i + "(I)), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "DoubleRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Double(" + i + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else if (symbolTable.getArraySType(id, i) == "FloatRep") {
            doIndentation()
            out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            doIndentation()
            out.println("XDR.Encode(XDR.XDR_Float(" + i + "(I), Message.Payload, Position, Last);")
            doIndentation()
            out.println("Position := Last + 1;")
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getST(id, i).contains("EnumRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + s + "'Pos(" + i + ")), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("StringRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + i + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + i + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("DataRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + i + "'Length), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("XDR.Encode(" + i + ", Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("UHyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned_Hyper(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("HyperRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Hyper(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getStructuredTypeParent(id, i) == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Boolean'Val(Boolean'Pos(" + i + ")), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getStructuredTypeParent(id, i) != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Boolean'Val(" + symbolTable.getStructuredTypeParent(id, i) + "'Pos(" + i + ")), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("UIntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("IntRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Integer(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("DoubleRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Double(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("FloatRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Float(" + i + "), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(1000*Interval), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
        else if (symbolTable.getST(id, i).contains("TimeRep")) {
          doIndentation()
          out.println("XDR.Encode(XDR.XDR_Unsigned(Seconds), Message.Payload, Position, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
        }
      }
    }
    doIndentation()
    out.println("Message.Size := Position;")
    doIndentation()
    out.println("return Message;")
    indentationLevel -= 1
    doIndentation()
    out.println("end " + id + encodeString + ";")
    out.println("")
    null
  }

  //Decode sending.
  def doDecode(ctx: XDRParser.Struct_bodyContext, id: String, arrowFlag: Int): Void = {
    val decodeString = "_Decode"
    doIndentation()
    out.println("procedure " + id + decodeString)
    indentationLevel += 1
    doIndentation()
    out.println("(Message : in  Message_Record;")
    val structStuff = ctx.declaration().size()
    for (i <- 0 until structStuff) {
      doIndentation()
      val t = symbolTable.getST(id, ctx.declaration(i).IDENTIFIER().getText)
      val tt = ctx.declaration(i).children.get(0).getText
      val idd = ctx.declaration(i).IDENTIFIER().getText
      val ts = if (ctx.declaration(i).children.contains(ctx.declaration(i).type_specifier())) {
        ctx.declaration(i).type_specifier().getText
      }
      if (symbolTable.getTypeNames.exists(_ == ts)) {
        out.println(idd + " : out " + ts + ";")
      }
      else {
        if (t.contains("StringRep")) {
          out.println(idd + " : out String;")
          doIndentation()
          out.println(idd + "_Size : out " + "Natural;")
        }
        else if (t.contains("DataRep")) {
          out.println(idd + " : out CubedOS.Lib.Octet_Array;")
          doIndentation()
          out.println("Size : out CubedOS.Lib.Octet_Array_Count;")
        }
        else {
          if (tt == "opaque") {
            out.println(idd + " : out CubedOS.Lib.Octet_Array;")
            doIndentation()
            out.println("Size : out CubedOS.Lib.Octet_Array_Count;")
          }
          else if (tt == "int") {
            out.println(idd + " : out Integer;")
          }
          else if (tt == "unsignedhyper") {
            out.println(id + " : out Lib.U_Hyper_Type;")
          }
          else if (tt == "unsignedint") {
            out.println(id + " : out Lib.Quadruple_Octet;")
          }
          else if (tt == "double") {
            out.println(idd + " : out Double;")
          }
          else if (tt == "float") {
            out.println(idd + " : out Float;")
          }
          else if (tt == "hyper") {
            out.println(idd + " : out Lib.Hyper_Type;")
          }
          else if (tt == "bool") {
            out.println(idd + " : out Boolean;")
          }
          else if (t == "string") {
            out.println(idd + " : String;")
          }
          else {
            out.println(idd + " : out " + ctx.declaration(i).type_specifier().getText + ";")
          }
        }
      }
    }
    doIndentation()
    out.println("Decode_Status : out Message_Status_Type)")
    indentationLevel -= 1
    doIndentation()
    out.println("is")
    indentationLevel += 1
    doIndentation()
    out.println("Position : XDR_Index_Type;")
    for (i <- symbolTable.getSType(id)) {
      var s = symbolTable.getStructuredTypeParent(id, i)
      if (symbolTable.getST(id, i).contains("StructRep")) {
        var l = List[String]()
        l = l.:+(i)
        processStructDF(s, l, 0)
      }
      else if (symbolTable.getST(id, i).contains("ArrayRep")) {
        if (symbolTable.getArraySType(id, i) == "StructRep") {
          var l = List[String]()
          l = l.:+(i)
          val structNum = symbolTable.getArraySSize(id, i).toInt
          processStructDF(s, l, structNum)
        }
        else if (symbolTable.getArraySType(id, i) == "EnumRep") {
          doIndentation()
          out.println("Raw_" + i + " : XDR.XDR_Unsigned;")
        }
        else if (symbolTable.getArraySType(id, i) == "StringRep") {
          doIndentation()
          out.println("Raw_" + i + "_Size : XDR.XDR_Unsigned;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Boolean;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Boolean;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Hyper;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Unsigned_Hyper;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Hyper;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Double;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Double;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Float;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Float;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Integer;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("Raw_" + i + "   : XDR.XDR_Integer;")
        }
      }
      else if (symbolTable.getST(id, i).contains("EnumRep")) {
        doIndentation()
        out.println("Raw_" + i + " : XDR.XDR_Unsigned;")
      }
      else if (symbolTable.getST(id, i).contains("StringRep")) {
        doIndentation()
        out.println("Raw_" + i + "_Size : XDR.XDR_Unsigned;")
      }
      else if (symbolTable.getST(id, i).contains("DataRep")) {
        doIndentation()
        out.println("Raw_Size : XDR.XDR_Unsigned;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Boolean;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Boolean;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned_Hyper;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Hyper;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned_Hyper;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Hyper;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Double;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Double;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Float;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Float;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Integer;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Integer;")
      }
      else if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
        doIndentation()
        out.println("Raw_Interval  : XDR.XDR_Unsigned;")
      }
      else if (symbolTable.getST(id, i).contains("TimeRep")) {
        doIndentation()
        out.println("Seconds : Ada.Real_Time.Seconds_Count;")
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
      }
      else {
        doIndentation()
        out.println("Raw_" + i + "   : XDR.XDR_Unsigned;")
      }
    }
    doIndentation()
    out.println("Last : XDR_Index_Type;")
    indentationLevel -= 1
    doIndentation()
    out.println("begin")
    indentationLevel += 1
    doIndentation()
    out.println("pragma Warnings")
    indentationLevel += 1
    doIndentation()
    out.println("""(Off, "unused assignment to ""Last""" + """""""" + """"""" + """, Reason => "The last value of Last is not needed");""")
    indentationLevel -= 1
    doIndentation()
    out.println("Decode_Status := Success;")
    for (i <- symbolTable.getSType(id)) {
      var s = symbolTable.getStructuredTypeParent(id, i)
      if (symbolTable.getST(id, i).contains("StructRep")) {
        var l = List[String]()
        l = l :+ i
        processStructV(s, l, 0)
      }
      else if (symbolTable.getST(id, i).contains("ArrayRep")) {
        if (symbolTable.getArraySType(id, i) == "StructRep") {
          var l = List[String]()
          l = l :+ i
          val structNum = symbolTable.getArraySSize(id, i).toInt
          doIndentation()
          out.println("for Y in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          processStructV(s, l, structNum)
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (symbolTable.getArraySType(id, i) == "StringRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := (others => ' ');")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Boolean'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.Hyper_Type(XDR.XDR_Hyper'First);")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Double'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Float'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Integer(XDR.XDR_Integer'First);")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
      }
      else if (symbolTable.getST(id, i).contains("StringRep")) {
        doIndentation()
        out.println(i + " := (others => ' ');")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("EnumRep")) {
        // The EnumRep case was added by pchapin... Do we need a case for s == "null"?
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println(i + " := Boolean'First;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println(i + " := Lib.U_Hyper_Type(XDR.XDR_Unsigned_Hyper'First);")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println(i + " := Lib.Hyper_Type(XDR.XDR_Hyper'First);")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println(i + " := Double'First;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println(i + " := Float'First;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println(i + " := Lib.Quadruple_Octet(XDR.XDR_Unsigned'First);")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println(i + " := Integer(XDR.XDR_Integer'First);")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println(i + " := " + s + "'First;")
      }
      else if (symbolTable.getST(id, i).contains("DataRep")) {
        doIndentation()
        out.println(i + " := (others => 0);")
        doIndentation()
        out.println("Size := 0;")
      }
      else if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
        doIndentation()
        out.println(i + " := Ada.Real_Time.Time_Span_First;")
      }
      else if (symbolTable.getST(id, i).contains("TimeRep")) {
        doIndentation()
        out.println(i + " := Ada.Real_Time.Time_First;")
      }
    }
    doIndentation()
    out.println("Position := 0;")
    val num = ctx.declaration().size()
    for (i <- symbolTable.getSType(id)) {
      var s = symbolTable.getStructuredTypeParent(id, i)
      if (symbolTable.getST(id, i).contains("StructRep")) {
        var l = List[String]()
        l = l.:+(i)
        var ss = List[String]()
        ss = ss.:+(i)
        val sL = digEndingStructs(s, ss)
        var stringFlag = ""
        for (u <- sL) {
          if (u != "") {
            stringFlag = stringFlag.concat(u)
            stringFlag = stringFlag.concat("_")
          }
        }
        if (i == symbolTable.getSType(id).last) {
          val posFlag = 1
          processStructD(id, s, l, posFlag, stringFlag, 0)
        }
        else {
          val posFlag = 0
          processStructD(id, s, l, posFlag, stringFlag, 0)
        }
      }
      else if (symbolTable.getST(id, i).contains("ArrayRep")) {
        if (symbolTable.getArraySType(id, i) == "StructRep") {
          val structNum = symbolTable.getArraySSize(id, i).toInt
          var l = List[String]()
          l = l.:+(i)
          var ss = List[String]()
          ss = ss.:+(i)
          val sL = digEndingStructs(s, ss)
          var stringFlag = ""
          for (u <- sL) {
            if (u != "") {
              stringFlag = stringFlag.concat(u)
              stringFlag = stringFlag.concat("_")
            }
          }
          if (i == symbolTable.getSType(id).last) {
            val posFlag = 1
            doIndentation()
            out.println("for Y in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            processStructD(id, s, l, posFlag, stringFlag, structNum)
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
          else {
            val posFlag = 0
            doIndentation()
            out.println("for Y in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
            indentationLevel += 1
            processStructD(id, s, l, posFlag, stringFlag, structNum)
            indentationLevel -= 1
            doIndentation()
            out.println("end loop;")
          }
        }
        else if (symbolTable.getArraySType(id, i) == "EnumRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in " + s +
            "'Pos(" + s + "'First) .. " +
            s + "'Pos(" + s +
            "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I)" + " := " + s + "'Val(Raw_" + i + ");")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          doIndentation()
          out.println(i + "(I)" + " := " + s + "'First;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (symbolTable.getArraySType(id, i) == "StringRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + "_Size, Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i +
            "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "_Size(I) := Natural(Raw_" + i + "_Size);")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println(i + "_Size(I) := 0;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          doIndentation()
          out.println("if " + i + "_Size(I) < 1 then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, " + i + "(" + i + "'First .. " + i +
            "'First + (" + i + "_Size(I) - 1)), Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println(i + "(I) := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "BoolRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println(i + "(I) := " + s + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Unsigned_Hyper(" + s +
            "'First) .. XDR.XDR_Unsigned_Hyper(" + s + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UHyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.U_Hyper_Type(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Hyper(" + s +
            "'First) .. XDR.XDR_Hyper(" + s + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "HyperRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Hyper(Lib.Hyper_Type'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.Hyper_Type(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Double(Double'First) .. XDR.XDR_Double(Double'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Double(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "DoubleRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Double(" + s +
            "'First) .. XDR.XDR_Double(" + s + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Float(Float'First) .. XDR.XDR_Float(Float'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Float(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "FloatRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Float(" + s +
            "'First) .. XDR.XDR_Float(" + s + "'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "UIntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Lib.Quadruple_Octet(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s != "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := " + s + "(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
        else if (s == "null" && symbolTable.getArraySType(id, i) == "IntRep") {
          doIndentation()
          out.println("for I in Integer range 0 .. " + symbolTable.getArraySSize(id, i) + " loop")
          indentationLevel += 1
          doIndentation()
          out.println("if Decode_Status = Success then")
          indentationLevel += 1
          doIndentation()
          out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
          doIndentation()
          out.println("Position := Last + 1;")
          doIndentation()
          out.println("if Raw_" + i + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
          indentationLevel += 1
          doIndentation()
          out.println(i + "(I) := Integer(Raw_" + i + ");")
          doIndentation()
          out.println("Decode_Status := Success;")
          indentationLevel -= 1
          doIndentation()
          out.println("else")
          indentationLevel += 1
          doIndentation()
          out.println("Decode_Status := Malformed;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end if;")
          indentationLevel -= 1
          doIndentation()
          out.println("end loop;")
        }
      }
      else if (symbolTable.getST(id, i).contains("EnumRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in " + s +
          "'Pos(" + s + "'First) .. " +
          s + "'Pos(" + s +
          "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "'Val(Raw_" + i + ");")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        doIndentation()
        out.println(i + " := " + s + "'First;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (symbolTable.getST(id, i).contains("StringRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + "_Size, Last);")
        doIndentation()
        out.println("Position := Last + 1;")
        doIndentation()
        out.println("if Raw_" + i +
          "_Size in XDR.XDR_Unsigned(Natural'First) .. XDR.XDR_Unsigned(Natural'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + "_Size := Natural(Raw_" + i + "_Size);")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println(i + "_Size := 0;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        doIndentation()
        out.println("if " + i + "_Size < 1 then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, " + i + "(" + i + "'First .. " + i +
          "'First + (" + i + "_Size - 1)), Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println(i + " := Boolean'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("BoolRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println(i + " := " + s + "'Val(XDR.XDR_Boolean'Pos(Raw_" + i + "));")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Unsigned_Hyper(" + s +
          "'First) .. XDR.XDR_Unsigned_Hyper(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UHyperRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'First) .. XDR.XDR_Unsigned_Hyper(Lib.U_Hyper_Type'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Lib.U_Hyper_Type(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Hyper(" + s +
          "'First) .. XDR.XDR_Hyper(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("HyperRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Hyper(Lib.Hyper_Type'First) .. XDR.XDR_Hyper(Lib.Hyper_Type'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Lib.Hyper_Type(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Double(Double'First) .. XDR.XDR_Double(Double'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Double(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("DoubleRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Double(" + s +
          "'First) .. XDR.XDR_Double(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Float(Float'First) .. XDR.XDR_Float(Float'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Float(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("FloatRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Float(" + s +
          "'First) .. XDR.XDR_Float(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Unsigned(" + s + "'First) .. XDR.XDR_Unsigned(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("UIntRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Unsigned(Lib.Quadruple_Octet'First) .. XDR.XDR_Unsigned(Lib.Quadruple_Octet'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Lib.Quadruple_Octet(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s != "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Integer(" + s + "'First) .. XDR.XDR_Integer(" + s + "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (s == "null" && symbolTable.getST(id, i).contains("IntRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in XDR.XDR_Integer(Integer'First) .. XDR.XDR_Integer(Integer'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Integer(Raw_" + i + ");")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (symbolTable.getST(id, i).contains("DataRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_Size, Last);")
        doIndentation()
        out.println("Position := Last + 1;")
        doIndentation()
        out.println("if Raw_Size in XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'First) .. XDR.XDR_Unsigned(CubedOS.Lib.Octet_Array_Count'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println("Size := CubedOS.Lib.Octet_Array_Count(Raw_Size);")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        doIndentation()
        out.println("if Size < " + i + "'Length then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, " + i + "(" + i + "'First .. " + i + "'First + Size - 1), Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (symbolTable.getST(id, i).contains("TimeSpanRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_Interval, Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_Interval < XDR.XDR_Unsigned(Integer'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := Ada.Real_Time.Milliseconds(Integer(Raw_Interval));")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (symbolTable.getST(id, i).contains("TimeRep")) {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " < XDR.XDR_Unsigned(Integer'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println("Seconds := Ada.Real_Time.Seconds_Count(Raw_" + i + ");")
        doIndentation()
        out.println(i + " := Ada.Real_Time.Time_Of(Seconds, Ada.Real_Time.Time_Span_Zero);")
        doIndentation()
        out.println("Decode_Status := Success;")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
      else if (symbolTable.getST(id, i).contains("VoidRep")) {
        //do nothing!
      }
      else {
        doIndentation()
        out.println("if Decode_Status = Success then")
        indentationLevel += 1
        doIndentation()
        out.println("XDR.Decode(Message.Payload, Position, Raw_" + i + ", Last);")
        if (i != ctx.declaration(num - 1).IDENTIFIER().getText) {
          doIndentation()
          out.println("Position := Last + 1;")
        }
        doIndentation()
        out.println("if Raw_" + i + " in " + s +
          "'Pos(" + s + "'First) .. " +
          s + "'Pos(" + s +
          "'Last) then")
        indentationLevel += 1
        doIndentation()
        out.println(i + " := " + s + "'Val(Raw_" + i + ");")
        indentationLevel -= 1
        doIndentation()
        out.println("else")
        indentationLevel += 1
        doIndentation()
        out.println("Decode_Status := Malformed;")
        doIndentation()
        out.println(i + " := " + s + "'First;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
        indentationLevel -= 1
        doIndentation()
        out.println("end if;")
      }
    }
    indentationLevel -= 1
    doIndentation()
    out.println("end " + id + decodeString + ";")
    null
  }

  //Process structs at end of struct element list (nested structs).
  def digEndingStructs(id: String, l: List[String]): List[String] = {
    var ll = List[String]()
    ll = l
    for (u <- symbolTable.getSType(id)) {
      if (u == symbolTable.getSType(id).last) {
        if (symbolTable.getST(id, u).contains("StructRep")) {
          ll = ll.:+(u)
          ll = digEndingStructs(symbolTable.getStructuredTypeParent(id, u), ll)
        }
      }
    }
    ll
  }

  override def visitType_def(ctx: XDRParser.Type_defContext): Void = {
    val typeOfTypeDef = ctx.getChild(0)
    typeOfTypeDef.getText match {
      case "message" =>
        var voidFlag = 0
        for (i <- 0 until ctx.struct_body().declaration().size()) {
          if (ctx.struct_body().declaration(i).getText == "void") {
            voidFlag = 1
          }
        }
        if (voidFlag == 1 && ctx.struct_body().declaration().size() > 1) {
          println("Can't have multiple message struct")
          println("parameters included with void.")
        }
        else if (voidFlag == 1 && ctx.struct_body().declaration().size() == 1) {
          val n = ctx.IDENTIFIER().getText
          var arrowFlag = 0
          if (ctx.children.contains(ctx.LARROW())) {
            arrowFlag = 1
          }
          doEncode(ctx.struct_body(), n, arrowFlag)
          out.println("")
        }
        else {
          val n = ctx.IDENTIFIER().getText
          var arrowFlag = 0
          if (ctx.children.contains(ctx.LARROW())) {
            arrowFlag = 1
          }
          doEncode(ctx.struct_body(), n, arrowFlag)
          doDecode(ctx.struct_body(), n, arrowFlag)
          out.println("")
        }
      case _ =>
    }
    null
  }
}
