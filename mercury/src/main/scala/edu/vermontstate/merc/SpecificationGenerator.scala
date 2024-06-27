package edu.vermontstate.merc

import java.io.File

import TypeRep.IntRep
import edu.vermontstate.merc.MXDRParser.*

class SpecificationGenerator(
  templateFolder : String,
  nameOfFile     : String,
  symbolTable    : BasicSymbolTable,
  out            : java.io.PrintStream,
  reporter       : Reporter) extends MXDRBaseVisitor[Void] {

  // The number of indentations where output lines start.
  private var indentationLevel = 0

  private def doIndentation(): Unit = {
    for (i <- 0 until indentationLevel) {
      out.print("   ")
    }
  }

  private def insertLine(bulk: List[String], str: String, indicator: String): List[String] = {
    var newBulk = List[String]()
    for (line <- bulk) {
      newBulk = line :: newBulk
      if (line.contains(indicator)) {
        newBulk = ("\n" + str) :: newBulk
      }
    }
    newBulk.reverse
  }

  def addedLines(lines: List[String]): List[String] = {
    var flagTimer = 0
    for (i <- symbolTable.getMStructs) {
      for (ii <- symbolTable.getSType(i)) {
        val s = symbolTable.getST(i, ii)
        if (s.contains("TimeSpanRep") ||
          s.contains("TimeRep")) {
          flagTimer = 1
        }
      }
    }
    var l = List[String]()
    if (flagTimer == 1) {
      val s1 = "with Ada.Real_Time;"
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
    val source = scala.io.Source.fromFile(templateFolder + File.separator + "template.ads")
    val lines = source.getLines().toList
    val newLines = addedLines(lines)
    source.close()
    newLines
  }

  override def visitSpecification(ctx: MXDRParser.SpecificationContext): Void = {
    val replacementString = nameOfFile
    val lines = processTemplate()
    for (line <- lines) {
      val newLine = line.replace("%MODULENAME%", replacementString)
      if (line.contains("%BULK%")) {
        indentationLevel += 1
        doIndentation()
        out.println("type Message_Type is")
        indentationLevel += 1
        doIndentation()
        out.print("(")
        val n = symbolTable.getMStructs
        for (name <- n) {
          if (name == n.last) {
            if (n.size == 1) {
              out.println(name + ");")
            }
            else {
              doIndentation()
              out.println(name + ");")
            }
          }
          else {
            if (name == n.head) {
              out.println(name + ", ")
            }
            else {
              doIndentation()
              out.println(name + ", ")
            }
          }
        }
        out.println("")
        indentationLevel -= 1
        visitChildren(ctx)
        indentationLevel -= 1
      }
      else {
        out.println(newLine)
      }
    }
    null
  }

  override def visitDefinition(ctx: DefinitionContext): Void = {
    visitChildren(ctx)
    null
  }

  override def visitLine(ctx: LineContext): Void = {
    doIndentation()
    visitDeclaration(ctx.declaration())
    out.println(";")
    out.println("")
    null
  }

  override def visitDeclaration(ctx: DeclarationContext): Void = {
    out.print("type ")
    val n = if (ctx.children.contains(ctx.IDENTIFIER)) {
      if (ctx.children.contains(ctx.LBRACKET) && ctx.children.contains(ctx.RBRACKET)) {
        if (ctx.type_specifier.children.contains(ctx.type_specifier.struct_type_spec)) {
          ctx.IDENTIFIER.getText + "_Intermediary"
        }
        else if (ctx.type_specifier.children.contains(ctx.type_specifier.enum_type_spec)) {
          ctx.IDENTIFIER.getText + "_Intermediary"
        }
        else {
          ctx.IDENTIFIER.getText
        }
      }
      else {
        ctx.IDENTIFIER.getText
      }
    }
    else {
      ctx.VOID.getText
    }
    out.print(n)
    if (!ctx.children.contains(ctx.VOID)) {
      out.print(" is")
      ctx.start.getText match {
        case "opaque" =>
          out.print(" new CubedOS.Lib.Octet_Array")
        case "string" =>
          out.print(" new String")
        case "enum" =>
          if (ctx.children.contains(ctx.LBRACKET) && ctx.children.contains(ctx.RBRACKET)) {
            val id = ctx.IDENTIFIER.getText
            out.println("")
            visitType_specifier(ctx.type_specifier)
            out.println(";")
            out.println("")
            doIndentation()
            out.print("type " + id + " is array (0 .. " + ctx.value.CONSTANT.toString + ") of " + id + "_Intermediary")
          }
          else {
            out.println("")
            visitType_specifier(ctx.type_specifier)
          }
        case "struct" =>
          if (ctx.children.contains(ctx.LBRACKET) && ctx.children.contains(ctx.RBRACKET)) {
            val id = ctx.IDENTIFIER.getText
            indentationLevel += 1
            out.println("")
            doIndentation()
            out.println("record")
            visitStruct_body(ctx.type_specifier.struct_type_spec.struct_body, id)
            doIndentation()
            out.println("end record;")
            out.println("")
            indentationLevel -= 1
            doIndentation()
            out.print("type " + id + " is array (0 .. " + ctx.value.CONSTANT.toString + ") of " + id + "_Intermediary")
          }
          else {
            val id = ctx.IDENTIFIER.getText
            indentationLevel += 1
            out.println("")
            doIndentation()
            out.println("record")
            visitStruct_body(ctx.type_specifier.struct_type_spec.struct_body, id)
            doIndentation()
            out.print("end record")
            indentationLevel -= 1
          }
        case _ =>
          if (ctx.getChild(0).getText == n || ctx.getChild(0).getText == n.toString.substring(0, n.toString.indexOf("_") + 1)) {
            out.print(" " + n)
          }
          else {
            if (ctx.children.contains(ctx.LBRACKET) && ctx.children.contains(ctx.RBRACKET)) {
              visitType_specifier(ctx.type_specifier)
            }
            else {
              visitType_specifier2(ctx.type_specifier)
            }
            if (ctx.children.contains(ctx.CONSTANT)) {
              out.print(" := " + ctx.CONSTANT.getText)
            }
          }
        /*case "union" =>*/
      }
    }
    null
  }

  override def visitValue(ctx: ValueContext): Void = {
    if (ctx.children.contains(ctx.CONSTANT)) {
      out.print(ctx.CONSTANT.getText)
    }
    else if (ctx.children.contains(ctx.IDENTIFIER)) {
      out.print(ctx.IDENTIFIER.getText)
    }
    null
  }

  def visitType_specifier2(ctx: Type_specifierContext): Void = {
    ctx.getStart.getText match {
      case "int" => out.print(" new Integer")
      case "unsigned" => if (ctx.children.contains(ctx.INT)) {
        out.print(" new Lib.Quadruple_Octet")
      }
      else if (ctx.children.contains(ctx.HYPER)) {
        out.print(" new Lib.U_Hyper_Type")
      }
      case "hyper" => out.print(" new Lib.Hyper_Type")
      case "float" => out.print(" new Float")
      case "double" => out.print(" new Double")
      case "quadruple" => out.print(" new Quadruple")
      case "bool" => out.print(" new Boolean")
      case "string" => out.print(" new String")
      case "enum" => visitEnum_type_spec(ctx.enum_type_spec)
      case "struct" => visitStruct_type_spec(ctx.struct_type_spec)
      case "union" => visitUnion_type_spec(ctx.union_type_spec)
      case _ =>
        out.print(" new " + ctx.getText)
    }
    null
  }

  override def visitType_specifier(ctx: Type_specifierContext): Void = {
    ctx.getStart.getText match {
      case "int" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Integer")
      case "unsigned" => if (ctx.children.contains(ctx.INT)) {
        out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Lib.Quadruple_Octet")
      }
      else if (ctx.children.contains(ctx.HYPER)) {
        out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Lib.U_Hyper_Type")
      }
      case "hyper" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Lib.Hyper_Type")
      case "float" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Float")
      case "double" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Double")
      case "quadruple" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Quadruple")
      case "bool" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Boolean")
      case "string" => out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of String")
      case "enum" => visitEnum_type_spec(ctx.enum_type_spec)
      case "struct" =>
        visitStruct_type_spec(ctx.struct_type_spec)
        out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of " + ctx.parent.getChild(1).getText + "_Intermediary")
      case "union" => visitUnion_type_spec(ctx.union_type_spec)
      case _ =>
        if (symbolTable.getTypeRepresentation(ctx.parent.getChild(1).getText).toString.contains("ArrayRep")) {
          if (symbolTable.getArrayType(ctx.parent.getChild(1).getText) == IntRep || ctx.children.contains(ctx.INT())) {
            out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of Integer")
          }
          else if (ctx.children.contains(ctx.IDENTIFIER)) {
            out.print(" array (0 .. " + symbolTable.getArraySize(ctx.parent.getChild(1).getText) + ") of " + ctx.IDENTIFIER())
          }
        }
    }
    null
  }

  override def visitEnum_type_spec(ctx: Enum_type_specContext): Void = {
    visitEnum_body(ctx.enum_body)
    null
  }

  override def visitEnum_body(ctx: MXDRParser.Enum_bodyContext): Void = {
    val enumeratorList = ctx.IDENTIFIER
    indentationLevel += 1
    doIndentation()
    out.print("(")
    if (!ctx.getChild(2).getText.equals(",")) {
      val v = ctx.IDENTIFIER.size
      for (i <- 0 until v) {
        val enumerator = ctx.IDENTIFIER(i).getText
        var vr = ""
        if (ctx.children.contains(ctx.value)) {
          if (ctx.value(i).children.contains(ctx.value(i).CONSTANT)) {
            vr = ctx.value(i).CONSTANT.getText
          }
          else {
            vr = ctx.value(i).IDENTIFIER.getText
          }
          out.print(enumerator + " = " + vr)
        }
        else {
          out.print(enumerator)
        }
        if (i < (v - 1)) {
          out.println(", ")
          doIndentation()
        }
      }
    }
    else {
      val it = enumeratorList.iterator
      while (it.hasNext) {
        val enumerator = it.next()
        out.print(enumerator.getText)
        if (it.hasNext) {
          out.println(", ")
          doIndentation()
        }
      }
    }
    indentationLevel -= 1
    out.print(")")
    null
  }

  override def visitStruct_type_spec(ctx: Struct_type_specContext): Void = {
    visitStruct_body(ctx.struct_body)
    null
  }

  //Decode.
  def doDecode(ctx: MXDRParser.Struct_bodyContext, id: String, m_i: List[String], arrowFlag: Int): Void = {
    val decodeString = "_Decode"
    doIndentation()
    out.println("procedure " + id + decodeString)
    indentationLevel += 1
    doIndentation()
    out.println("(Message : in  Message_Record;")
    val structStuff = ctx.declaration.size()
    for (i <- 0 until structStuff) {
      doIndentation()
      val t = ctx.declaration(i).children.get(0).getText
      val id = ctx.declaration(i).IDENTIFIER.getText
      if (t == "opaque") {
        out.println(id + " : out CubedOS.Lib.Octet_Array;")
        doIndentation()
        out.println("Size : out CubedOS.Lib.Octet_Array_Count;")
      }
      else if (t == "string") {
        out.println(id + " : out String;")
        doIndentation()
        out.println(id + "_Size : out " + "Natural;")
      }
      else if (t == "CubedOS.Lib.Octet_Array") {
        out.println(id + " : out " + ctx.declaration(i).type_specifier.getText + ";")
        doIndentation()
        out.println("Size : out CubedOS.Lib.Octet_Array_Count;")
      }
      else if (t == "int") {
        out.println(id + " : out Integer;")
      }
      else if (t == "unsignedhyper") {
        out.println(id + " : out Lib.U_Hyper_Type;")
      }
      else if (t == "unsignedint") {
        out.println(id + " : out Lib.Quadruple_Octet;")
      }
      else if (t == "double") {
        out.println(id + " : out Double;")
      }
      else if (t == "float") {
        out.println(id + " : out Float;")
      }
      else if (t == "hyper") {
        out.println(id + " : out Lib.Hyper_Type;")
      }
      else if (t == "bool") {
        out.println(id + " : out Boolean;")
      }
      else {
        out.println(id + " : out " + ctx.declaration(i).type_specifier.getText + ";")
      }
    }
    doIndentation()
    out.println("Decode_Status : out Message_Status_Type)")
    indentationLevel -= 1
    doIndentation()
    out.println("with")
    indentationLevel += 1
    doIndentation()
    out.println("Global => null,")
    doIndentation()
    out.println("Pre => Is_" + id + "(Message),")
    doIndentation()
    out.print("Depends => ((")
    for (i <- 0 until structStuff) {
      val t = if (ctx.declaration(i).children.contains(ctx.declaration(i).type_specifier)) {
        ctx.declaration(i).type_specifier.getText
      }
      else if (ctx.declaration(i).children.contains(ctx.declaration(i).OPAQUE)) {
        "CubedOS.Lib.Octet_Array"
      }
      else if (ctx.declaration(i).children.contains(ctx.declaration(i).STRING)) {
        "string"
      }
      else {
        ""
      }
      val id = ctx.declaration(i).IDENTIFIER.getText
      if (i == structStuff - 1) {
        if (m_i.isEmpty) {
          if (t == "string" ||
            ctx.declaration(i).children.contains(ctx.declaration(i).STRING)) {
            out.print(id + ", ")
            out.print(id + "_Size, Decode_Status) => Message);")
          }
          else if (t == "CubedOS.Lib.Octet_Array" || t == "opaque") {
            out.print(id + ", ")
            out.print("Size, Decode_Status) => Message);")
          }
          else {
            out.print(id + ", Decode_Status) => Message);")
          }
        }
        else {
          if (t == "string" ||
            ctx.declaration(i).children.contains(ctx.declaration(i).STRING)) {
            out.print(id + ", ")
            out.print(id + "_Size, Decode_Status) => Message),")
          }
          else if (t == "CubedOS.Lib.Octet_Array" || t == "opaque") {
            out.print(id + ", ")
            out.print("Size, Decode_Status) => Message),")
          }
          else {
            out.print(id + ", Decode_Status) => Message),")
          }
        }
      }
      else {
        if (t == "string" ||
          ctx.declaration(i).children.contains(ctx.declaration(i).STRING)) {
          out.print(id + ", ")
          out.print(id + "_Size, ")
        }
        else if (t == "CubedOS.Lib.Octet_Array" || t == "opaque") {
          out.print(id + ", ")
          out.print("Size, ")
        }
        else {
          out.print(id + ", ")
        }
      }
    }
    out.println("")
    for (i <- m_i.indices) {
      if (i == m_i.size - 1) {
        doIndentation()
        out.println("Post => " + m_i(i) + ";")
      }
      else {
        doIndentation()
        out.println("Post => " + m_i(i) + ",")
      }
    }
    indentationLevel -= 1
    out.println("")
    null
  }

  //Encode sending.
  def doEncode(ctx: MXDRParser.Struct_bodyContext, id: String, m_i: List[String], arrowFlag: Int): Void = {
    val encodeString = "_Encode"
    var stringFlag = ""
    var dataFlag = ""
    doIndentation()
    out.println("function " + id + encodeString)
    indentationLevel += 1
    doIndentation()
    out.print("(")
    if (arrowFlag == 0) {
      out.println("Sender_Domain : Domain_ID_Type;")
      doIndentation()
      out.println("Sender  : Module_ID_Type;")
      doIndentation()
      out.println("Request_ID : Request_ID_Type;")
    }
    else if (arrowFlag == 1) {
      out.println("Receiver_Domain : Domain_ID_Type;")
      doIndentation()
      out.println("Receiver  : Module_ID_Type;")
      doIndentation()
      out.println("Request_ID : Request_ID_Type;")
    }
    val structStuff = ctx.declaration().size()
    for (i <- 0 until structStuff) {
      if (ctx.declaration(i).children.contains(ctx.declaration(i).VOID)) {
        doIndentation()
        out.println("--TODO")
      }
      else {
        doIndentation()
        val t = ctx.declaration(i).children.get(0).getText
        val id = ctx.declaration(i).IDENTIFIER.getText
        if (t == "string") {
          out.println(id + " : String;")
          stringFlag = id
        }
        else if (t == "CubedOS.Lib.Octet_Array") {
          out.println(id + " : " + ctx.declaration(i).type_specifier.getText + ";")
          dataFlag = id
        }
        else if (t == "opaque") {
          out.println(id + " : CubedOS.Lib.Octet_Array;")
          dataFlag = id
        }
        else if (t == "int") {
          out.println(id + " : Integer;")
        }
        else if (t == "unsignedhyper") {
          out.println(id + " : Lib.U_Hyper_Type;")
        }
        else if (t == "unsignedint") {
          out.println(id + " : Lib.Quadruple_Octet;")
        }
        else if (t == "double") {
          out.println(id + " : Double;")
        }
        else if (t == "float") {
          out.println(id + " : Float;")
        }
        else if (t == "hyper") {
          out.println(id + " : Lib.Hyper_Type;")
        }
        else if (t == "bool") {
          out.println(id + " : Boolean;")
        }
        else {
          out.println(id + " : " + ctx.declaration(i).type_specifier.getText + ";")
        }
      }
    }
    doIndentation()
    out.println("Priority : System.Priority := System.Default_Priority) return Message_Record")
    indentationLevel -= 1
    doIndentation()
    out.println("with")
    indentationLevel += 1
    doIndentation()
    if (stringFlag != "") {
      if (m_i.isEmpty) {
        out.println("Global => null,")
        doIndentation()
        out.println("Pre => (0 < " + stringFlag + "'Length and " + stringFlag + "'Length <= XDR_Size_Type'Last - 12);")
      }
      else {
        out.println("Global => null,")
        doIndentation()
        out.println("Pre => (0 < " + stringFlag + "'Length and " + stringFlag + "'Length <= XDR_Size_Type'Last - 12),")
      }
    }
    else {
      if (m_i.isEmpty) {
        out.println("Global => null;")
      }
      else {
        out.println("Global => null,")
        for (i <- m_i.indices) {
          if (i == m_i.size - 1) {
            doIndentation()
            out.println("Pre => " + m_i(i) + ";")
          }
          else {
            doIndentation()
            out.println("Pre => " + m_i(i) + ",")
          }
        }
      }
    }
    indentationLevel -= 1
    out.println("")
    null
  }

  //Check sending.
  def doCheck(id: String, arrowFlag: Int): Void = {
    val checkString = "Is_"
    doIndentation()
    out.println("function " + checkString + id + "(Message : Message_Record) return Boolean is")
    indentationLevel += 1
    doIndentation()
    if (arrowFlag == 0) {
      out.print("(Message.Receiver")
    }
    else if (arrowFlag == 1) {
      out.print("(Message.Sender")
    }
    out.println(" = ID and Message.Message_ID = Message_Type'Pos(" + id + "));")
    out.println("")
    indentationLevel -= 1
    null
  }

  def visitStruct_body(ctx: Struct_bodyContext, id: String): Void = {
    indentationLevel += 1
    for (i <- 0 until ctx.declaration.size()) {
      doIndentation()
      out.print(ctx.declaration(i).IDENTIFIER.getText)
      out.print(" : ")
      symbolTable.getST(id, ctx.declaration(i).IDENTIFIER.getText) match {
        case "UIntRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.INT)) {
          out.print("Lib.Quadruple_Octet")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "IntRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.INT)) {
          out.print("Integer")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "FloatRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.FLOAT)) {
          out.print("Float")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "DoubleRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.DOUBLE)) {
          out.print("Double")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "UHyperRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.HYPER)) {
          out.print("Lib.U_Hyper_Type")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "HyperRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.HYPER)) {
          out.print("Lib.Hyper_Type")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "BoolRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.BOOL)) {
          out.print("Boolean")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "StringRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.STRING)) {
          out.print("String")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "TimeSpanRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.TIME_SPAN)) {
          out.print("Ada.Real_Time.Time_Span")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "TimeRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.TIME)) {
          out.print("Ada.Real_Time.Time")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "DataRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.DATA)) {
          out.print("CubedOS.Lib.Octet_Array")
        }
        else {
          out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
        }
        case "EnumRep" =>
          val y = ctx.declaration(i).IDENTIFIER.getText
          out.print(symbolTable.getStructuredTypeParent(id, y))
        case "StructRep" =>
          val y = ctx.declaration(i).IDENTIFIER.getText
          out.print(symbolTable.getStructuredTypeParent(id, y))
        case "ArrayRep" =>
          val y = ctx.declaration(i).IDENTIFIER.getText
          symbolTable.getArraySType(id, y) match {
            case "UIntRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.INT)) {
              out.print("Lib.Quadruple_Octet")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "IntRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.INT)) {
              out.print("Integer")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "FloatRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.FLOAT)) {
              out.print("Float")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "DoubleRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.DOUBLE)) {
              out.print("Double")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "UHyperRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.HYPER)) {
              out.print("Lib.U_Hyper_Type")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "HyperRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.HYPER)) {
              out.print("Lib.Hyper_Type")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "BoolRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.BOOL)) {
              out.print("Boolean")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "StringRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.STRING)) {
              out.print("String")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "TimeSpanRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.TIME_SPAN)) {
              out.print("Ada.Real_Time.Time_Span")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "TimeRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.TIME)) {
              out.print("Ada.Real_Time.Time")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "DataRep" => if (ctx.declaration(i).type_specifier.children.contains(ctx.declaration(i).type_specifier.DATA)) {
              out.print("CubedOS.Lib.Octet_Array")
            }
            else {
              out.print(symbolTable.getStructuredTypeParent(id, ctx.declaration(i).IDENTIFIER.getText))
            }
            case "EnumRep" =>
              val y = ctx.declaration(i).IDENTIFIER.getText
              out.print(symbolTable.getStructuredTypeParent(id, y))
            case "StructRep" =>
              val y = ctx.declaration(i).IDENTIFIER.getText
              out.print(symbolTable.getStructuredTypeParent(id, y))
            case _ =>
              out.print(ctx.declaration(i).type_specifier.getText)
          }
        case _ =>
          out.print(ctx.declaration(i).type_specifier.getText)
      }
      out.println(";")
    }
    indentationLevel -= 1
    null
  }

  override def visitConstant_def(ctx: Constant_defContext): Void = {
    doIndentation()
    val value = ctx.CONSTANT.getText
    if (ctx.getChildCount == 5) {
      val id = ctx.IDENTIFIER(0).getText
      out.println(id + ": constant := " + value + ";" + "\n")
    }
    else if (ctx.getChildCount == 7) {
      val id = ctx.IDENTIFIER(0).getText
      out.println(id + ": constant " + ctx.IDENTIFIER(1).getText + " := " + value + ";" + "\n")
    }
    null
  }

  override def visitType_def(ctx: MXDRParser.Type_defContext): Void = {
    val typeOfTypeDef = ctx.getChild(0).getText
    typeOfTypeDef match {
      case "typedef" =>
        doIndentation()
        if (!ctx.children.contains(ctx.subtype_spec)) {
          visitDeclaration(ctx.declaration)
          if (ctx.children.contains(ctx.RANGE)) {
            val t = ctx.declaration().IDENTIFIER.getText
            if (symbolTable.getTypeRepresentation(t).equals(TypeRep.FloatRep)) {
              visitRange_constraint(ctx.range_constraint, 1)
            }
            else {
              visitRange_constraint(ctx.range_constraint, 0)
            }
          }
        }
        else {
          out.print("subtype " + ctx.declaration.IDENTIFIER.getText + " is ")
          if (ctx.range_constraint.children.contains(ctx.range_constraint.IDENTIFIER)) {
            if (ctx.range_constraint.IDENTIFIER.getText.contains('\'')) {
              out.print(ctx.range_constraint.IDENTIFIER.
                getText.substring(0, ctx.range_constraint.IDENTIFIER.
                getText.indexOf('\'')))
            }
            else {
              out.print("Natural")
            }
          }
          else {
            out.print(ctx.subtype_spec.IDENTIFIER)
          }
          val t = ctx.declaration.IDENTIFIER.getText
          if (symbolTable.getTypeRepresentation(t).equals(TypeRep.FloatRep)) {
            visitRange_constraint(ctx.range_constraint, 1)
          }
          else {
            visitRange_constraint(ctx.range_constraint, 0)
          }
        }
        out.println(";")
        out.println("")

      case "enum"
      =>
        doIndentation()
        out.println("type " + ctx.IDENTIFIER.getText + " is ")
        indentationLevel += 1
        visitEnum_body(ctx.enum_body)
        indentationLevel -= 1
        out.println(";")
        out.println("")

      case "struct"
      =>
        val id = ctx.IDENTIFIER.getText
        doIndentation()
        out.println("type " + id + " is ")
        indentationLevel += 1
        doIndentation()
        out.println("record")
        visitStruct_body(ctx.struct_body, id)
        doIndentation()
        out.println("end record;")
        indentationLevel -= 1
        out.println("")

      case "union"
      =>
        val unionNode = ctx.UNION
        reporter.reportError(
          unionNode.getSymbol.getLine,
          unionNode.getSymbol.getCharPositionInLine + 1,
          "Union definitions not yet implemented")

      case "message"
      =>
        val n = ctx.IDENTIFIER.getText
        var m_i = List[String]()
        if (ctx.children.contains(ctx.condition)) {
          for (i <- 0 until ctx.condition.expression.size()) {
            if (ctx.condition.expression(i).children.contains(ctx.condition.expression(i).GOE)) {
              m_i = (ctx.condition.expression(i).IDENTIFIER(0).getText + " " +
                ctx.condition.expression(i).GOE.getText + " " +
                ctx.condition.expression(i).IDENTIFIER(1).getText) :: m_i
            }
            else if (ctx.condition.expression(i).children.contains(ctx.condition.expression(i).LOE)) {
              m_i = (ctx.condition.expression(i).IDENTIFIER(0).getText + " " +
                ctx.condition.expression(i).LOE.getText + " " +
                ctx.condition.expression(i).IDENTIFIER(1).getText) :: m_i
            }
            else if (ctx.condition.expression(i).children.contains(ctx.condition.expression(i).RANGLE)) {
              m_i = (ctx.condition.expression(i).IDENTIFIER(0).getText + " " +
                ctx.condition.expression(i).RANGLE.getText + " " +
                ctx.condition.expression(i).IDENTIFIER(1).getText) :: m_i
            }
            else if (ctx.condition.expression(i).children.contains(ctx.condition.expression(i).LANGLE)) {
              m_i = (ctx.condition.expression(i).IDENTIFIER(0).getText + " " +
                ctx.condition.expression(i).LANGLE.getText + " " +
                ctx.condition.expression(i).IDENTIFIER(1).getText) :: m_i
            }
            else if (ctx.condition.expression(i).children.contains(ctx.condition.expression(i).EQUALS)) {
              m_i = (ctx.condition.expression(i).IDENTIFIER(0).getText + " " +
                ctx.condition.expression(i).EQUALS.getText + " " +
                ctx.condition.expression(i).IDENTIFIER(1).getText) :: m_i
            }
          }
        }
        var voidFlag = 0
        for (i <- 0 until ctx.struct_body.declaration.size()) {
          if (ctx.struct_body.declaration(i).getText == "void") {
            voidFlag = 1
          }
        }
        if (voidFlag == 1 && ctx.struct_body.declaration.size() > 1) {
          println("Can't have multiple message struct")
          println("parameters included with void.")
        }
        else if (voidFlag == 1 && ctx.struct_body.declaration.size() == 1) {
          var arrowFlag = 0
          if (ctx.children.contains(ctx.LARROW)) {
            arrowFlag = 1
          }
          doEncode(ctx.struct_body(), n, m_i, arrowFlag)
          doCheck(n, arrowFlag)
          out.println("")
        }
        else {
          var arrowFlag = 0
          if (ctx.children.contains(ctx.LARROW)) {
            arrowFlag = 1
          }
          doEncode(ctx.struct_body, n, m_i, arrowFlag)
          doCheck(n, arrowFlag)
          doDecode(ctx.struct_body, n, m_i, arrowFlag)
          out.println("")
        }
      case _ =>
    }
    null
  }

  def visitRange_constraint(ctx: MXDRParser.Range_constraintContext, floatFlag: Int): Void

  = {
    var lowerBound = ctx.getChild(0).getText
    var upperBound = ctx.getChild(2).getText
    if (ctx.children.contains(ctx.IDENTIFIER)) {
      if (ctx.IDENTIFIER.getText.contains("'")) {
        val n = ctx.IDENTIFIER.getText.indexOf("'")
        if (symbolTable.getTypeNames.exists(_ == ctx.IDENTIFIER.getText.substring(0, n))) {
          val t = symbolTable.getTypeRepresentation(ctx.IDENTIFIER.getText.substring(0, n))
          if (t.equals(TypeRep.UIntRep)) {
            var f = ctx.IDENTIFIER.getText
            while (f.contains("'")) {
              val num = f.indexOf("'")
              if (symbolTable.getTypeNames.exists(_ == f.substring(0, num))) {
                f = symbolTable.getTypeValue(f.substring(0, num))
                f = f.substring(f.lastIndexOf(" ") + 1, f.length)
              }
            }
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.equals(TypeRep.IntRep)) {
            var f = ctx.IDENTIFIER.getText
            while (f.contains("'")) {
              val num = f.indexOf("'")
              if (symbolTable.getTypeNames.exists(_ == f.substring(0, num))) {
                f = symbolTable.getTypeValue(f.substring(0, num))
                f = f.substring(f.lastIndexOf(" ") + 1, f.length)
              }
            }
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.toString.contains("ConstRep")) {
            var f = symbolTable.getTypeValue(ctx.IDENTIFIER.getText)
            f = f.substring(f.lastIndexOf(" ") + 1, f.length)
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.equals(TypeRep.FloatRep)) {
            var f = ctx.IDENTIFIER.getText
            while (f.contains("'")) {
              val num = f.indexOf("'")
              if (symbolTable.getTypeNames.exists(_ == f.substring(0, num))) {
                f = symbolTable.getTypeValue(f.substring(0, num))
                f = f.substring(f.lastIndexOf(" ") + 1, f.length)
              }
            }
            if (!lowerBound.contains(".")){
              lowerBound = lowerBound + ".0"
            }
            if (!f.contains(".")){
              f = f + ".0"
            }
            out.print(" range " + lowerBound + " .. " + f)
          }
          else {
            reporter.reportError(
              ctx.IDENTIFIER.getSymbol.getLine,
              ctx.IDENTIFIER.getSymbol.getCharPositionInLine + 1,
              upperBound.substring(0, n) + " hasn't been declared yet.")
          }
        }
      }
      else {
        if (upperBound.toLowerCase == "natural") {
          out.print(" range " + lowerBound + " .. 4294967295")
        }
        else if (symbolTable.getTypeNames.exists(_ == ctx.IDENTIFIER.getText)) {
          val t = symbolTable.getTypeRepresentation(ctx.IDENTIFIER.getText)
          if (t.equals(TypeRep.UIntRep)) {
            var f = symbolTable.getTypeValue(ctx.IDENTIFIER.getText)
            f = f.substring(f.lastIndexOf(" ") + 1, f.length)
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.equals(TypeRep.IntRep)) {
            var f = symbolTable.getTypeValue(ctx.IDENTIFIER.getText)
            f = f.substring(f.lastIndexOf(" ") + 1, f.length)
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.toString.contains("ConstRep")) {
            var f = symbolTable.getTypeValue(ctx.IDENTIFIER.getText)
            f = f.substring(f.lastIndexOf(" ") + 1, f.length)
            out.print(" range " + lowerBound + " .. " + f)
          }
          else if (t.equals(TypeRep.FloatRep)) {
            var f = symbolTable.getTypeValue(ctx.IDENTIFIER.getText)
            f = f.substring(f.lastIndexOf(" ") + 1, f.length)
            if (!lowerBound.contains(".")){
              lowerBound = lowerBound + ".0"
            }
            if (!f.contains(".")){
              f = f + ".0"
            }
            out.print(" range " + lowerBound + " .. " + f)
          }
          else {
            reporter.reportError(
              ctx.IDENTIFIER.getSymbol.getLine,
              ctx.IDENTIFIER.getSymbol.getCharPositionInLine + 1,
              upperBound + " hasn't been declared yet.")
          }
        }
      }
    }
    else {
      if (floatFlag == 1) {
        if (!lowerBound.contains(".")){
          lowerBound = lowerBound + ".0"
        }
        if (!upperBound.contains(".")){
          upperBound = upperBound + ".0"
        }
        out.print(" range " + lowerBound + " .. " + upperBound)
      }
      else {
        out.print(" range " + lowerBound + " .. " + upperBound)
      }
    }
    null
  }
}
