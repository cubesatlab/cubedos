package edu.vtc.xdr2os3

import edu.vtc.xdr2os3
import edu.vtc.xdr2os3.TypeRep.{ComponentRep, UnionRep}

import scala.util.control.Breaks._

class SemanticAnalyzer(
                        nameOfFile : String,
                        symbolTable: BasicSymbolTable,
                        reporter   : Reporter) extends XDRBaseVisitor[(String, TypeRep.Rep, String)] {


  /*override def visitMessage_struct_type_spec(ctx: XDRParser.Message_struct_type_specContext): TypeRep.Rep = {
    val MSName = ctx.IDENTIFIER().getText
    val parameters = visitChildren(ctx).asInstanceOf[ComponentRep]
    symbolTable.addObjectName(MSName, TypeRep.MStructRep(MSName, parameters), "null")
    TypeRep.MStructRep(MSName, parameters)
  }

  override def visitStruct_body(ctx: XDRParser.Struct_bodyContext): TypeRep.Rep = {
    visitChildren(ctx).asInstanceOf[ComponentRep]
  }

  override def visitType_def(ctx: XDRParser.Type_defContext): TypeRep.Rep = {
    if (ctx.children.contains(ctx.IDENTIFIER())) {
      val Name = ctx.IDENTIFIER().getText
      val components = visitChildren(ctx).asInstanceOf[ComponentRep]

      symbolTable.addObjectName(Name,
        ctx.getChild(0).getText match {
          case "ENUM" => TypeRep.EnumRep(Name, components)
          case "STRUCT" => TypeRep.StructRep(Name, components)
          case "UNION" => TypeRep.UnionRep(Name, components)
        }
        ,"null")
    }
    else {
      if (ctx.getChild(0).getText.equalsIgnoreCase("TYPEDEF")){
        if (ctx.children.contains(ctx.range_constraint())){
          val Name = visitDeclaration(ctx.declaration())
          val value = visitRange_constraint2(ctx.range_constraint())
          // TODO: Create an appropriate TypeRep that holds the range constraint
          // TODO: Typedefs don't define objects so addObjectName isn't appropriate here.
          symbolTable.addObjectName(Name.toString, TypeRep.TypeDefRep, value)
        }
      }
      visitChildren(ctx).asInstanceOf[ComponentRep]
    }
    TypeRep.NoTypeRep
  }

  override def visitDeclaration(ctx: XDRParser.DeclarationContext): TypeRep.Rep = {
   //TODO
    TypeRep.NoTypeRep
  }

  def visitType_specifier2(ctx: XDRParser.Type_specifierContext): String = {
    if (ctx.children.contains(ctx.enum_type_spec())){
      visitEnum_type_spec(ctx.enum_type_spec())
      "null"
    }
    else if (ctx.children.contains(ctx.message_struct_type_spec())){
      visitMessage_struct_type_spec(ctx.message_struct_type_spec())
      "null"
    }
    else if (ctx.children.contains(ctx.struct_type_spec())){
      visitStruct_type_spec(ctx.struct_type_spec())
      "null"
    }
    else if (ctx.children.contains(ctx.union_type_spec())){
      visitUnion_type_spec(ctx.union_type_spec())
      "null"
    }
    else {
      ctx.getText
    }
  }

  def visitRange_constraint2(ctx: XDRParser.Range_constraintContext): String = {
    val first = ctx.CONSTANT(0)
    val second = ctx.CONSTANT(1)
    val dotdot = ctx.DOTDOT()
    val constraint = first.toString + dotdot.toString + second.toString
    constraint
  }*/

  /*override def enterMessage_struct_type_spec(ctx: XDRParser.Message_struct_type_specContext): Unit = {
    val MSName = ctx.IDENTIFIER().getText
    val parameters: List[TypeRep.Rep]

    for (i <- 0 until ctx.struct_body().declaration().size()) {
      parameters.::(ctx.struct_body().declaration(i))
    }

    symbolTable.addObjectName(MSName, XDRParser.Message_struct_type_specContext)

    /*val typedefCount = ctx.struct_body.type_def.size
    val constCount = ctx.struct_body.constant_def.size
    val decCount = ctx.struct_body.declaration.size
    val sid = ctx.IDENTIFIER.getText

    for (i <- 0 until typedefCount) {
      if (symbolTable.contains(ctx.struct_body.declaration(i).IDENTIFIER.getText)) {
        val (id, tO, v) = enterStruct_body(i, decCount, constCount, typedefCount, ctx.struct_body)
        val adder = SymbolInfo(sid, id.toString, tO.toString, v.toString)
        symbolTable + sid -> adder
      }
    }

    for (i <- 0 until constCount) {
      if (symbolTable.contains(ctx.struct_body.declaration(i).IDENTIFIER.getText)) {
        val (id, tO, v) = enterStruct_body(i, decCount, constCount, typedefCount, ctx.struct_body)
        val adder = SymbolInfo(sid, id.toString, tO.toString, v.toString)
        symbolTable + sid -> adder
      }
    }

    for (i <- 0 until decCount) {
      if (symbolTable.contains(ctx.struct_body.declaration(i).IDENTIFIER.getText)) {
        val (id, tO, v) = enterStruct_body(i, decCount, constCount, typedefCount, ctx.struct_body)
        val adder = SymbolInfo(sid, id.toString, tO.toString, v.toString)
        symbolTable + sid -> adder
      }
    }*/
  }

  override def enterStruct_body(ctx: XDRParser.Struct_bodyContext): Unit = {

   /* if (decCount > 0) {
      val id = ctx.declaration(i).IDENTIFIER().getText
      val tO = ctx.declaration(i).type_specifier().getText
      val v = ctx.declaration(i).CONSTANT().getText
      (id, tO, v)
    }*/
    /*else if (ctx.getChild(1).getText == "struct_type_spec") {

  }
  else if (ctx.getChild(1).getText == "enum_type_spec") {

  }
  else if (ctx.getChild(1).getText == "union_type_spec") {

  }*/
   /* else if (constCount > 0) {
      val (id, tO, v) = enterConstant_def2(ctx.constant_def(i))
      (id, tO, v)
    }
    else if (typedefCount > 0) {
      val (id, tO, v) = enterType_def2(ctx.type_def(i))
      (id, tO, v)
    }*/
  }


  def enterConstant_def2(ctx: XDRParser.Constant_defContext): Any = {
    if (ctx.IDENTIFIER().size() > 1) {
      val sid = "null"
      val id = ctx.IDENTIFIER(0).toString
      val tO = ctx.IDENTIFIER(1).getText
      val v = ctx.CONSTANT.getText

      val adder = SymbolInfo(sid, id, tO, v)

      symbolTable + id -> adder
      (id, tO, v)
    }
    else {
      val sid = "null"
      val id = ctx.IDENTIFIER(0).toString
      val tO = id
      val v = ctx.CONSTANT.getText

      val adder = SymbolInfo(sid, id, tO, v)

      symbolTable + id -> adder
      (id, tO, v)
    }
  }

  def enterType_def2(ctx: XDRParser.Type_defContext): Any = {
    if (ctx.getChild(0).getText == "typedef" && ctx.declaration().getChild(0).getText != "opaque") {
      if (ctx.range_constraint().getChild(2).getText.contains(XDRParser.CONSTANT)) {
        val sid = "null"
        val id = ctx.declaration().IDENTIFIER().getText
        val tO = ctx.declaration().type_specifier().getText
        val v = ctx.range_constraint().CONSTANT(2).getText

        val adder = SymbolInfo(sid, id, tO, v)

        symbolTable + id -> adder
        (id, tO, v)
      }
    }
  }


  // Make sure range constraints are non-empty.
  override def exitRange_constraint(ctx: XDRParser.Range_constraintContext): Unit = {
    val lowerBoundNode = ctx.CONSTANT(0)
    if (ctx.CONSTANT().size() > 1) {
      val upperBoundNode = ctx.CONSTANT(1)
      val lowerBound = lowerBoundNode.getText.toInt
      val upperBound = upperBoundNode.getText.toInt

      if (upperBound < lowerBound) {
        reporter.reportError(
          ctx.DOTDOT.getSymbol.getLine,
          ctx.DOTDOT.getSymbol.getCharPositionInLine + 1,
          "Invalid range constraint: Require lower bound <= upper bound")
      }
    }
    else {
      var upperBound = 0
      val upperBoundNode = ctx.IDENTIFIER().getText
      val lowerBound = lowerBoundNode.getText.toInt
      var search = ""
      if (upperBoundNode.contains(''')) {
        val index = upperBoundNode.indexOf(''')
        search = upperBoundNode.substring(0, index)
      }
      else {
        search = upperBoundNode
      }
      if (search.compareToIgnoreCase("natural") == 0) {
        upperBound = 99999999
      }
      else {
        var bool = true
        var which = false
        while (bool) {
          breakable {
            val noOfStatements = ctx.parent.parent.parent.getChildCount
            for (i <- 0 until noOfStatements) {
              val matcher = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(0).getText
              if (ctx.parent.parent.parent.getChild(i).getChild(0).getText.contains(search)) {
                matcher match {
                  case "typedef" =>
                    if (ctx.parent.parent.parent.getChild(i).getChild(0).getChildCount == 5) {
                      if (ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getChild(2).getText.length > 3) {
                        search = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getChild(2).getText
                        which = true
                        break
                      }
                      else {
                        upperBound = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getChild(2).getText.toInt
                        which = false
                        break
                      }
                    }
                    else if (ctx.parent.parent.parent.getChild(i).getChild(0).getChildCount == 6) {
                      if (ctx.parent.parent.parent.getChild(i).getChild(0).getChild(4).getChild(2).getText.length > 3) {
                        search = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(4).getChild(2).getText
                        which = true
                        break
                      }
                      else {
                        upperBound = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(4).getChild(2).getText.toInt
                        which = false
                        break
                      }
                    }

                  case "const" =>
                    if (ctx.parent.parent.parent.getChild(i).getChild(0).getChildCount == 5) {
                      if (ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getText.length > 3) {
                        search = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getText
                        which = true
                        break
                      }
                      else {
                        upperBound = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(3).getText.toInt
                        which = false
                        break
                      }
                    }
                    else if (ctx.parent.parent.parent.getChild(i).getChild(0).getChildCount == 7) {
                      if (ctx.parent.parent.parent.getChild(i).getChild(0).getChild(5).getText.length > 3) {
                        search = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(5).getText
                        which = true
                        break
                      }
                      else {
                        upperBound = ctx.parent.parent.parent.getChild(i).getChild(0).getChild(5).getText.toInt
                        which = false
                        break
                      }
                    }
                }
              }
            }
          }
          if (!which) {
            bool = false
          }
          else {
            bool = true
          }
        }
      }

      if (upperBound < lowerBound) {
        reporter.reportError(
          ctx.DOTDOT.getSymbol.getLine,
          ctx.DOTDOT.getSymbol.getCharPositionInLine + 1,
          "Invalid range constraint: Require lower bound <= upper bound")
        println("")
      }
    }
  }*/

}
