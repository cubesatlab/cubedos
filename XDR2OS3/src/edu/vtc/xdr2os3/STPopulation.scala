package edu.vtc.xdr2os3

import edu.vtc.xdr2os3
import edu.vtc.xdr2os3.TypeRep.{ComponentRep, UnionRep}

import scala.util.control.Breaks._

class STPopulation(
                    nameOfFile : String,
                    symbolTable: BasicSymbolTable,
                    reporter   : Reporter) extends XDRBaseVisitor[(String, TypeRep.Rep, String)] {

  def visitRC(ctx: XDRParser.Range_constraintContext): String = {
    val lowerBoundNode = ctx.CONSTANT(0)
    if (ctx.CONSTANT().size() > 1) {
      val upperBoundNode = ctx.CONSTANT(1)
      val lowerBound = lowerBoundNode.getText.toDouble
      val upperBound = upperBoundNode.getText.toDouble

      if (upperBound < lowerBound) {
        reporter.reportError(
          ctx.DOTDOT.getSymbol.getLine,
          ctx.DOTDOT.getSymbol.getCharPositionInLine + 1,
          "Invalid range constraint: Require lower bound <= upper bound")
      }
      return lowerBound.toString + " .. " + upperBound.toString
    }
    else {
      var upperBound = 0.0
      val upperBoundNode = ctx.IDENTIFIER().getText
      val lowerBound = lowerBoundNode.getText.toDouble
      var search = ""
      if (upperBoundNode.contains('\'')) {
        val index = upperBoundNode.indexOf('\'')
        search = upperBoundNode.substring(0, index)
      }
      else {
        search = upperBoundNode
      }
      if (search.compareToIgnoreCase("natural") == 0) {
        upperBound = 2147483647
      }
      else {
        val n = symbolTable.getTypeNames
        for (i <- n) {
          if (i == search) {
            val index = symbolTable.getTypeValue(i).lastIndexOf(" ") + 1
            if (index > 0) {
              val num = symbolTable.getTypeValue(i).substring(index)
              upperBound = num.toDouble
            }
            else {
              upperBound = symbolTable.getTypeValue(i).toDouble
            }
          }
        }
        /*var bool = true
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
        }*/
      }

      if (upperBound < lowerBound) {
        reporter.reportError(
          ctx.DOTDOT.getSymbol.getLine,
          ctx.DOTDOT.getSymbol.getCharPositionInLine + 1,
          "Invalid range constraint: Require lower bound <= upper bound")
        println("")
      }
      return lowerBound.toString + " .. " + upperBound.toString
    }
  }

  def structP(ctx: XDRParser.Struct_bodyContext): ComponentRep = {
    var p2 = List[(String, TypeRep.Rep, String)]()
    val i = ctx.declaration().size()
    for (x <- 0 until i) {
      if (ctx.declaration(x).children.contains(ctx.declaration(x).VOID())) {
        val t = TypeRep.VoidRep
        p2 = p2.:+(("null", t, "null"))
      }
      else {
        val name = ctx.declaration(x).IDENTIFIER()
        val n = ctx.declaration(x).IDENTIFIER().getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        var nameT = ""
        if (ctx.declaration(x).children.get(0).getText == "opaque") {
          nameT = "opaque"
        }
        else {
          nameT = ctx.declaration(x).type_specifier().getText
        }
        val c = if (ctx.declaration(x).children.contains(ctx.declaration(x).CONSTANT())) {
          ctx.declaration(x).CONSTANT().getText
        }
        else {
          "null"
        }
        ctx.declaration(x).start.getText match {
          case "CubedOS.Lib.Octet_Array" =>
            val t = TypeRep.DataRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "Ada.Real_Time.Time" =>
            val t = TypeRep.TimeRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "Ada.Real_Time.Time_Span" =>
            val t = TypeRep.TimeSpanRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "opaque" =>
            val t = TypeRep.OpaqueRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "string" =>
            val t = TypeRep.StringRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "void" =>
            val name = "void"
            val t = TypeRep.VoidRep
            p2 = p2.:+((name, t, c))
          case "int" =>
            val t = TypeRep.IntRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "unsigned" =>
            if (ctx.declaration(x).type_specifier().stop.getText == "int") {
              val t = TypeRep.UIntRep
              if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
                ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
                val s = ctx.declaration(x).value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                p2 = p2.:+((n, at, c))
              }
              else {
                p2 = p2.:+((n, t, c))
              }
            }
            else if (ctx.declaration(x).type_specifier().stop.getText == "hyper") {
              val t = TypeRep.UHyperRep
              if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
                ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
                val s = ctx.declaration(x).value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                p2 = p2.:+((n, at, c))
              }
              else {
                p2 = p2.:+((n, t, c))
              }
            }
          case "hyper" =>
            val t = TypeRep.HyperRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "float" =>
            val t = TypeRep.FloatRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "double" =>
            val t = TypeRep.DoubleRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "quadruple" =>
            val t = TypeRep.QuadRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "bool" =>
            val t = TypeRep.BoolRep
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t)
              p2 = p2.:+((n, at, c))
            }
            else {
              p2 = p2.:+((n, t, c))
            }
          case "enum" =>
            val t = TypeRep.EnumRep
            val p = enumP(ctx.declaration(x).type_specifier().enum_type_spec().enum_body())
            symbolTable.addSTypes(n, p)
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t.apply(n, p))
              p2 = p2.:+((n, at, c))
              symbolTable.addStructuredName(n, at, "null")
            }
            else {
              p2 = p2.:+((n, t.apply(n, p), c))
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
            }
            p2 = p2.:+((n, t.apply(n, p), "null"))
          case "struct" =>
            val t = TypeRep.StructRep
            val p = structP(ctx.declaration(x).type_specifier().struct_type_spec().struct_body())
            symbolTable.addSTypes(n, p)
            if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
              ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
              val s = ctx.declaration(x).value().CONSTANT().toString.toInt
              val at = TypeRep.ArrayRep(s, t.apply(n, p))
              p2 = p2.:+((n, at, c))
              symbolTable.addStructuredName(n, at, "null")
            }
            else {
              p2 = p2.:+((n, t.apply(n, p), c))
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
            }
            p2 = p2.:+((n, t.apply(n, p), "null"))
          case _ =>
            if (symbolTable.getStructuredTypeNames.exists(_ == nameT)) {
              val t = TypeRep.IDRep(symbolTable.getStructuredType(nameT))
              if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
                ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
                val s = ctx.declaration(x).value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                p2 = p2.:+((n, at, nameT))
              }
              else {
                p2 = p2.:+((n, t, nameT))
              }
            }
            else if (symbolTable.getTypeNames.exists(_ == nameT)) {
              val t = TypeRep.IDRep(symbolTable.getTypeRepresentation(nameT))
              if (ctx.declaration(x).children.contains(ctx.declaration(x).LBRACKET()) &&
                ctx.declaration(x).children.contains(ctx.declaration(x).RBRACKET())) {
                val s = ctx.declaration(x).value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                p2 = p2.:+((n, at, nameT))
              }
              else {
                p2 = p2.:+((n, t, nameT))
              }
            }
          /*case "union" =>
          val t = TypeRep.UnionRep
          val p = unionP(ctx.declaration().type_specifier(x).union_type_spec().union_body()).asInstanceOf[ComponentRep]
          symbolTable.addObjectName(name, t.apply(name, p), "null")
          (name, t.apply(name, p), "null")*/
        }
      }
    }
    ComponentRep(p2)
  }

  def enumP(ctx: XDRParser.Enum_bodyContext): ComponentRep = {
    var p2 = List[(String, TypeRep.Rep, String)]()
    val i = ctx.IDENTIFIER().size
    if (ctx.children.contains(ctx.EQUALS())) {
      for (x <- 0 until i) {
        val name = ctx.IDENTIFIER(x)
        val n = ctx.IDENTIFIER(x).getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        val t = TypeRep.IDERep(n)
        val c = ctx.value(x).CONSTANT().getText
        p2 = p2.:+((n, t, c))
      }
    }
    else {
      for (x <- 0 until i) {
        val name = ctx.IDENTIFIER(x)
        val n = ctx.IDENTIFIER(x).getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        val t = TypeRep.IDERep(n)
        val c = "null"
        p2 = p2.:+((n, t, c))
      }
    }
    ComponentRep(p2)
  }

  override def aggregateResult(aggregate: (String, TypeRep.Rep, String), nextResult: (String, TypeRep.Rep, String)): (String, TypeRep.Rep, String) = {
    val (n, t, v) = aggregate
    if (t.isInstanceOf[ComponentRep]) {
      val ComponentRep(components) = t.asInstanceOf[ComponentRep]
      (n, ComponentRep(components ::: List((nextResult._1, nextResult._2, nextResult._3))), v)
    }
    else {
      nextResult
    }
  }

  override def defaultResult(): (String, TypeRep.Rep, String) = {
    val d1 = "null"
    val d2 = "null"
    (d1, ComponentRep(List[(String, TypeRep.Rep, String)]()), d2)
  }

  override def visitSpecification(ctx: XDRParser.SpecificationContext): (String, TypeRep.Rep, String) = {
    visitChildren(ctx)
  }

  override def visitDefinition(ctx: XDRParser.DefinitionContext): (String, TypeRep.Rep, String) = {
    if (ctx.children.contains(ctx.type_def())) {
      ctx.getChild(0).getChild(0).getText match {
        case "typedef" =>
          if (symbolTable.checkTypes(ctx.type_def().declaration().IDENTIFIER().getText) == 1 ||
            symbolTable.checkSTypes(ctx.type_def().declaration().IDENTIFIER().getText) == 1) {
            reporter.reportError(
              ctx.type_def().declaration().IDENTIFIER().getSymbol.getLine,
              ctx.type_def().declaration().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
              "Invalid!: That Identifier already exists!")
            println("")
          }
          else {
            visitChildren(ctx)
          }
        case "enum" =>
          if (symbolTable.checkTypes(ctx.type_def().IDENTIFIER().getText) == 1 ||
            symbolTable.checkSTypes(ctx.type_def().IDENTIFIER().getText) == 1) {
            reporter.reportError(
              ctx.type_def().IDENTIFIER().getSymbol.getLine,
              ctx.type_def().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
              "Invalid!: That Identifier already exists!")
            println("")
          }
          else {
            visitChildren(ctx)
          }
        case "struct" =>
          if (symbolTable.checkTypes(ctx.type_def().IDENTIFIER().getText) == 1 ||
            symbolTable.checkSTypes(ctx.type_def().IDENTIFIER().getText) == 1) {
            reporter.reportError(
              ctx.type_def().IDENTIFIER().getSymbol.getLine,
              ctx.type_def().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
              "Invalid!: That Identifier already exists!")
            println("")
          }
          else {
            visitChildren(ctx)
          }
        case "message" =>
          if (symbolTable.checkTypes(ctx.type_def().IDENTIFIER().getText) == 1 ||
            symbolTable.checkSTypes(ctx.type_def().IDENTIFIER().getText) == 1) {
            reporter.reportError(
              ctx.type_def().IDENTIFIER().getSymbol.getLine,
              ctx.type_def().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
              "Invalid!: That Identifier already exists!")
            println("")
          }
          else {
            visitChildren(ctx)
          }
        case "const" => if (symbolTable.checkTypes(ctx.type_def().IDENTIFIER().getText) == 1 ||
          symbolTable.checkSTypes(ctx.type_def().IDENTIFIER().getText) == 1) {
          reporter.reportError(
            ctx.type_def().IDENTIFIER().getSymbol.getLine,
            ctx.type_def().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
            "Invalid!: That Identifier already exists!")
          println("")
        }
        else {
          visitChildren(ctx)
        }
      }
    }
    else if (ctx.children.contains(ctx.line())) {
      if (symbolTable.checkTypes(ctx.line().declaration().IDENTIFIER().getText) == 1 ||
        symbolTable.checkSTypes(ctx.line().declaration().IDENTIFIER().getText) == 1) {
        reporter.reportError(
          ctx.line().declaration().IDENTIFIER().getSymbol.getLine,
          ctx.line().declaration().IDENTIFIER().getSymbol.getCharPositionInLine + 1,
          "Invalid!: That Identifier already exists!")
        println("")
      }
      else {
        visitChildren(ctx)
      }
    }
    else if (ctx.children.contains(ctx.constant_def())) {
      if (symbolTable.checkTypes(ctx.constant_def().IDENTIFIER(0).getText) == 1 ||
        symbolTable.checkSTypes(ctx.constant_def().IDENTIFIER(0).getText) == 1) {
        reporter.reportError(
          ctx.constant_def().IDENTIFIER(0).getSymbol.getLine,
          ctx.constant_def().IDENTIFIER(0).getSymbol.getCharPositionInLine + 1,
          "Invalid!: That Identifier already exists!")
        println("")
      }
      else {
        visitChildren(ctx)
      }
    }
    ("null", TypeRep.NoTypeRep, "null")
  }

  override def visitDeclaration(ctx: XDRParser.DeclarationContext): (String, TypeRep.Rep, String) = {
    visitChildren(ctx)
  }

  override def visitType_def(ctx: XDRParser.Type_defContext): (String, TypeRep.Rep, String) = {
    ctx.getChild(0).getText match {
      case "typedef" =>
        val name = ctx.declaration().IDENTIFIER()
        val n = ctx.declaration().IDENTIFIER().getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        if (ctx.children.contains(ctx.range_constraint())) {
          val rc = visitRC(ctx.range_constraint())
          ctx.declaration().start.getText match {
            case "opaque" =>
              val t = TypeRep.OpaqueRep
              symbolTable.addTypeName(n, t, rc)
            case "string" =>
              val t = TypeRep.StringRep
              symbolTable.addTypeName(n, t, rc)
            case "void" =>
              val name = "void"
              val t = TypeRep.VoidRep
              symbolTable.addTypeName(name, t, "null")
            case "int" =>
              val t = TypeRep.IntRep
              symbolTable.addTypeName(n, t, rc)
            case "unsigned" =>
              if (ctx.declaration().type_specifier().getStop.getText == "int") {
                val t = TypeRep.UIntRep
                symbolTable.addTypeName(n, t, rc)
              }
              else if (ctx.declaration().type_specifier().getStop.getText == "hyper") {
                val t = TypeRep.UHyperRep
                symbolTable.addTypeName(n, t, rc)
              }
            case "hyper" =>
              val t = TypeRep.HyperRep
              symbolTable.addTypeName(n, t, rc)
            case "float" =>
              val t = TypeRep.FloatRep
              symbolTable.addTypeName(n, t, rc)
            case "double" =>
              val t = TypeRep.DoubleRep
              symbolTable.addTypeName(n, t, rc)
            case "quadruple" =>
              val t = TypeRep.QuadRep
              symbolTable.addTypeName(n, t, rc)
            case "bool" =>
              val t = TypeRep.BoolRep
              symbolTable.addTypeName(n, t, rc)
            case "enum" =>
              val t = TypeRep.EnumRep
              val p = enumP(ctx.declaration().type_specifier().enum_type_spec().enum_body())
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
              symbolTable.addSTypes(n, p)
              (n, t.apply(n, p), "null")
            case "struct" =>
              val t = TypeRep.StructRep
              val p = structP(ctx.declaration().type_specifier().struct_type_spec().struct_body())
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
              symbolTable.addSTypes(n, p)
              (name, t.apply(n, p), "null")
            case _ =>
              val t = TypeRep.IDRep(symbolTable.getTypeRepresentation(n))
              symbolTable.addTypeName(n, t, rc)
            /*case "union" =>
              val t = TypeRep.UnionRep
              val p = visitChildren(ctx.declaration().type_specifier().union_type_spec().union_body()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")*/
          }
        }

        else if (ctx.declaration().children.contains(ctx.declaration().CONSTANT())) {
          val c = ctx.declaration().CONSTANT().getText
          ctx.declaration().start.getText match {
            case "opaque" =>
              val t = TypeRep.OpaqueRep
              symbolTable.addTypeName(n, t, c)
            case "string" =>
              val t = TypeRep.StringRep
              symbolTable.addTypeName(n, t, c)
            case "void" =>
              val name = "void"
              val t = TypeRep.VoidRep
              symbolTable.addTypeName(n, t, "null")
            case "int" =>
              val t = TypeRep.IntRep
              symbolTable.addTypeName(n, t, c)
            case "unsigned" =>
              if (ctx.declaration().type_specifier().getStop.getText == "int") {
                val t = TypeRep.UIntRep
                symbolTable.addTypeName(n, t, c)
              }
              else if (ctx.declaration().type_specifier().getStop.getText == "hyper") {
                val t = TypeRep.UHyperRep
                symbolTable.addTypeName(n, t, c)
              }
            case "hyper" =>
              val t = TypeRep.HyperRep
              symbolTable.addTypeName(n, t, c)
            case "float" =>
              val t = TypeRep.FloatRep
              symbolTable.addTypeName(n, t, c)
            case "double" =>
              val t = TypeRep.DoubleRep
              symbolTable.addTypeName(n, t, c)
            case "quadruple" =>
              val t = TypeRep.QuadRep
              symbolTable.addTypeName(n, t, c)
            case "bool" =>
              val t = TypeRep.BoolRep
              symbolTable.addTypeName(n, t, c)
            case "enum" =>
              val t = TypeRep.EnumRep
              val p = enumP(ctx.declaration().type_specifier().enum_type_spec().enum_body())
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
              symbolTable.addSTypes(n, p)
              (n, t.apply(n, p), "null")
            case "struct" =>
              val t = TypeRep.StructRep
              val p = structP(ctx.declaration().type_specifier().struct_type_spec().struct_body())
              symbolTable.addStructuredName(n, t.apply(n, p), "null")
              symbolTable.addSTypes(n, p)
              (n, t.apply(n, p), "null")
            case _ =>
              val t = TypeRep.IDRep(symbolTable.getTypeRepresentation(n))
              symbolTable.addTypeName(n, t, c)
            /*case "union" =>
        val t = TypeRep.UnionRep
        val p = visitChildren(ctx.declaration().type_specifier().union_type_spec().union_body()).asInstanceOf[ComponentRep]
        symbolTable.addObjectName(name, t.apply(name, p), "null")
        (name, t.apply(name, p), "null")*/
          }
        }
        else {
          val c = "null"
          ctx.declaration().start.getText match {
            case "opaque" =>
              val t = TypeRep.OpaqueRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "string" =>
              val t = TypeRep.StringRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "void" =>
              val name = "void"
              val t = TypeRep.VoidRep
              symbolTable.addTypeName(n, t, "null")
            case "int" =>
              val t = TypeRep.IntRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "unsigned" =>
              if (ctx.declaration().type_specifier().getStop.getText == "int") {
                val t = TypeRep.UIntRep
                if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                  ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                  val s = ctx.declaration().value().CONSTANT().toString.toInt
                  val at = TypeRep.ArrayRep(s, t)
                  symbolTable.addTypeName(n, at, c)
                }
                else {
                  symbolTable.addTypeName(n, t, c)
                }
              }
              else if (ctx.declaration().type_specifier().getStop.getText == "hyper") {
                val t = TypeRep.UHyperRep
                if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                  ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                  val s = ctx.declaration().value().CONSTANT().toString.toInt
                  val at = TypeRep.ArrayRep(s, t)
                  symbolTable.addTypeName(n, at, c)
                }
                else {
                  symbolTable.addTypeName(n, t, c)
                }
              }
            case "hyper" =>
              val t = TypeRep.HyperRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "float" =>
              val t = TypeRep.FloatRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "double" =>
              val t = TypeRep.DoubleRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "quadruple" =>
              val t = TypeRep.QuadRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "bool" =>
              val t = TypeRep.BoolRep
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
            case "enum" =>
              val t = TypeRep.EnumRep
              val p = enumP(ctx.declaration().type_specifier().enum_type_spec().enum_body())
              symbolTable.addSTypes(n, p)
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t.apply(n, p))
                symbolTable.addTypeName(n, at, c)
                symbolTable.addStructuredName(n, at, "null")
              }
              else {
                symbolTable.addTypeName(n, t.apply(n, p), c)
                symbolTable.addStructuredName(n, t.apply(n, p), "null")
              }
              (n, t.apply(n, p), "null")
            case "struct" =>
              val t = TypeRep.StructRep
              val p = structP(ctx.declaration().type_specifier().struct_type_spec().struct_body())
              symbolTable.addSTypes(n, p)
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t.apply(n, p))
                symbolTable.addTypeName(n, at, c)
                symbolTable.addStructuredName(n, at, "null")
              }
              else {
                symbolTable.addTypeName(n, t.apply(n, p), c)
                symbolTable.addStructuredName(n, t.apply(n, p), "null")
              }
              (n, t.apply(n, p), "null")
            case _ =>
              val t = TypeRep.IDRep(symbolTable.getTypeRepresentation(ctx.declaration().type_specifier().getText))
              if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
                ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
                val s = ctx.declaration().value().CONSTANT().toString.toInt
                val at = TypeRep.ArrayRep(s, t)
                symbolTable.addTypeName(n, at, c)
              }
              else {
                symbolTable.addTypeName(n, t, c)
              }
          }
        }
        ("default", TypeRep.NoTypeRep, "null")
      case "enum"
      =>
        val name = ctx.IDENTIFIER()
        val n = ctx.IDENTIFIER().getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        val t = TypeRep.EnumRep
        val p = enumP(ctx.enum_body())
        symbolTable.addSTypes(n, p)
        symbolTable.addTypeName(n, t.apply(n, p), "null")
        symbolTable.addStructuredName(n, t.apply(n, p), "null")
        (n, t.apply(n, p), "null")
      case "struct"
      =>
        val name = ctx.IDENTIFIER()
        val n = ctx.IDENTIFIER().getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        val t = TypeRep.StructRep
        val p = structP(ctx.struct_body())
        symbolTable.addSTypes(n, p)
        symbolTable.addTypeName(n, t.apply(n, p), "null")
        symbolTable.addStructuredName(n, t.apply(n, p), "null")
        (n, t.apply(n, p), "null")
      /*case "union"
  =>
    val name = ctx.IDENTIFIER().getText
    val t = TypeRep.UnionRep
    val p = visitChildren(ctx.declaration().type_specifier().union_type_spec().union_body()).asInstanceOf[ComponentRep]
    symbolTable.addObjectName(name, t.apply(name, p), "null")
    (name, t.apply(name, p), "null")*/
      case "message"
      =>
        val name = ctx.IDENTIFIER()
        val n = ctx.IDENTIFIER().getText
        if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
          n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
          n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
          n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
          n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
          n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
          n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
          n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
          n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
          n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
          n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
          n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
          n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
          n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
          n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
          n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
          reporter.reportError(
            name.getSymbol.getLine,
            name.getSymbol.getCharPositionInLine + 1,
            name + " is a reserved SPARK Ada word.")
        }
        val t = TypeRep.MStructRep
        val p = structP(ctx.struct_body())
        symbolTable.addStructuredName(n, t.apply(n, p), "null")
        symbolTable.addSTypes(n, p)
        (n, t.apply(n, p), "null")
    }
  }


  override def visitLine(ctx: XDRParser.LineContext): (String, TypeRep.Rep, String) = {
    var n = if (ctx.declaration().children.contains(ctx.declaration().IDENTIFIER())) {
      ctx.declaration().IDENTIFIER().getText
    }
    else {
      ctx.declaration().VOID().getText
    }
    if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
      n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
      n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
      n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
      n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
      n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
      n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
      n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
      n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
      n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
      n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
      n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
      n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
      n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
      n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
      n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
      val name = ctx.declaration().IDENTIFIER()
      reporter.reportError(
        name.getSymbol.getLine,
        name.getSymbol.getCharPositionInLine + 1,
        name + " is a reserved SPARK Ada word.")
    }
    val c = if (ctx.declaration().children.contains(ctx.declaration().CONSTANT())) {
      ctx.declaration().CONSTANT().getText
    }
    else {
      "null"
    }
    ctx.declaration().start.getText match {
      case "opaque" =>
        val t = TypeRep.OpaqueRep
        symbolTable.addTypeName(n, t, c)
        (n, t, c)
      case "string" =>
        val t = TypeRep.StringRep
        symbolTable.addTypeName(n, t, c)
        (n, t, c)
      case "void" =>
        val name = "void"
        val t = TypeRep.VoidRep
        symbolTable.addTypeName(n, t, "null")
        (name, t, "null")
      case "int" =>
        val t = TypeRep.IntRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "unsigned" =>
        val t = if (ctx.declaration().type_specifier().getStop.getText == "int") {
          TypeRep.UIntRep
        }
        else if (ctx.declaration().type_specifier().getStop.getText == "hyper") {
          TypeRep.UHyperRep
        }
        else {
          TypeRep.NoTypeRep
          //throw error
        }
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "hyper" =>
        val t = TypeRep.HyperRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "float" =>
        val t = TypeRep.FloatRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "double" =>
        val t = TypeRep.DoubleRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "quadruple" =>
        val t = TypeRep.QuadRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "bool" =>
        val t = TypeRep.BoolRep
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      case "enum" =>
        val t = TypeRep.EnumRep
        val p = enumP(ctx.declaration().type_specifier().enum_type_spec().enum_body())
        symbolTable.addSTypes(n, p)
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t.apply(n, p))
          symbolTable.addTypeName(n, at, c)
          symbolTable.addStructuredName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t.apply(n, p), c)
          symbolTable.addStructuredName(n, t.apply(n, p), c)
        }
        (n, t.apply(n, p), "null")
      case "struct" =>
        val t = TypeRep.StructRep
        val p = structP(ctx.declaration().type_specifier().struct_type_spec().struct_body())
        symbolTable.addSTypes(n, p)
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t.apply(n, p))
          symbolTable.addTypeName(n, at, c)
          symbolTable.addStructuredName(n, at, c)

        }
        else {
          symbolTable.addTypeName(n, t.apply(n, p), c)
          symbolTable.addStructuredName(n, t.apply(n, p), c)
        }
        (n, t.apply(n, p), "null")
      case _ =>
        val t = TypeRep.IDRep(symbolTable.getTypeRepresentation(ctx.declaration().getChild(0).getText))
        if (ctx.declaration().children.contains(ctx.declaration().LBRACKET()) &&
          ctx.declaration().children.contains(ctx.declaration().RBRACKET())) {
          val s = ctx.declaration().value().CONSTANT().toString.toInt
          val at = TypeRep.ArrayRep(s, t)
          symbolTable.addTypeName(n, at, c)
        }
        else {
          symbolTable.addTypeName(n, t, c)
        }
        (n, t, c)
      /*case "union" =>
        val t = TypeRep.UnionRep
        val p = visitChildren(ctx.declaration().type_specifier().union_type_spec().union_body()).asInstanceOf[ComponentRep]
        symbolTable.addObjectName(name, t.apply(name, p), "null")
        (name, t.apply(name, p), "null")*/
    }
  }

  override def visitConstant_def(ctx: XDRParser.Constant_defContext): (String, TypeRep.Rep, String) = {
    val name = ctx.IDENTIFIER(0)
    val n = ctx.IDENTIFIER(0).getText
    if (n.toLowerCase == "abort" || n.toLowerCase == "abs" || n.toLowerCase == "abstract" || n.toLowerCase == "accept" ||
      n.toLowerCase == "access" || n.toLowerCase == "aliased" || n.toLowerCase == "all" || n.toLowerCase == "and" || n.toLowerCase == "array" ||
      n.toLowerCase == "at" || n.toLowerCase == "begin" || n.toLowerCase == "body" || n.toLowerCase == "case" || n.toLowerCase == "constant" ||
      n.toLowerCase == "declare" || n.toLowerCase == "delay" || n.toLowerCase == "delta" || n.toLowerCase == "digits" ||
      n.toLowerCase == "do" || n.toLowerCase == "else" || n.toLowerCase == "elsif" || n.toLowerCase == "end" || n.toLowerCase == "entry" ||
      n.toLowerCase == "exception" || n.toLowerCase == "exit" || n.toLowerCase == "for" || n.toLowerCase == "function" || n.toLowerCase == "generic" ||
      n.toLowerCase == "goto" || n.toLowerCase == "if" || n.toLowerCase == "in" || n.toLowerCase == "interface" ||
      n.toLowerCase == "is" || n.toLowerCase == "limited" || n.toLowerCase == "loop" || n.toLowerCase == "mod" || n.toLowerCase == "new" ||
      n.toLowerCase == "not" || n.toLowerCase == "null" || n.toLowerCase == "of" || n.toLowerCase == "or" || n.toLowerCase == "others" ||
      n.toLowerCase == "out" || n.toLowerCase == "overriding" || n.toLowerCase == "package" || n.toLowerCase == "pragma" ||
      n.toLowerCase == "private" || n.toLowerCase == "procedure" || n.toLowerCase == "protected" || n.toLowerCase == "raise" || n.toLowerCase == "range" ||
      n.toLowerCase == "record" || n.toLowerCase == "rem" || n.toLowerCase == "renames" || n.toLowerCase == "requeue" || n.toLowerCase == "return" ||
      n.toLowerCase == "reverse" || n.toLowerCase == "select" || n.toLowerCase == "separate" || n.toLowerCase == "some" ||
      n.toLowerCase == "subtype" || n.toLowerCase == "synchronized" || n.toLowerCase == "tagged" || n.toLowerCase == "task" || n.toLowerCase == "terminate" ||
      n.toLowerCase == "then" || n.toLowerCase == "type" || n.toLowerCase == "until" || n.toLowerCase == "use" || n.toLowerCase == "when" ||
      n.toLowerCase == "while" || n.toLowerCase == "with" || n.toLowerCase == "xor") {
      reporter.reportError(
        name.getSymbol.getLine,
        name.getSymbol.getCharPositionInLine + 1,
        name + " is a reserved SPARK Ada word.")
    }
    val t = TypeRep.ConstRep
    val v = ctx.CONSTANT().getText
    symbolTable.addTypeName(n, t.apply(n, v), v)
    (n, t.apply(n, v), v)
  }

  /*def unionP(ctx: XDRParser.Struct_bodyContext): List[(String, TypeRep.Rep, String)] = {
    var p2: List[(String, TypeRep.Rep, String)] = null
    val i = ctx.declaration().size()
    for (x <- 0 to i - 1) {
      val name = ctx.declaration(x).IDENTIFIER().getText
      val c = if (ctx.declaration(x).children.contains(ctx.declaration(x).CONSTANT())) {
        ctx.declaration(x).CONSTANT().getText
      }
      else {
        "null"
      }
      ctx.declaration(x).getChild(0).getText match {
        case "opaque" =>
          val t = TypeRep.OpaqueRep
          p2.::((name, t, c))
        case "string" =>
          val t = TypeRep.StringRep
          p2.::((name, t, c))
        case "void" =>
          val name = "void"
          val t = TypeRep.VoidRep
          (name, t, "null")
        case name =>
          val t = TypeRep.IDRep
          p2.::((name, t, c))
        case _ => ctx.declaration(x).type_specifier().getText match {
          case "int" =>
            val t = TypeRep.IntRep
            p2.::((name, t, c))
          case "unsignedint" =>
            val t = TypeRep.UIntRep
            p2.::((name, t, c))
          case "hyper" =>
            val t = TypeRep.HyperRep
            p2.::((name, t, c))
          case "unsignedhyper" =>
            val t = TypeRep.UHyperRep
            p2.::((name, t, c))
          case "float" =>
            val t = TypeRep.FloatRep
            p2.::((name, t, c))
          case "double" =>
            val t = TypeRep.DoubleRep
            p2.::((name, t, c))
          case "quadruple" =>
            val t = TypeRep.QuadRep
            p2.::((name, t, c))
          case "bool" =>
            val t = TypeRep.BoolRep
            p2.::((name, t, c))
          case "string" =>
            val t = TypeRep.StringRep
            p2.::((name, t, c))
          case name =>
            val t = TypeRep.IDRep
            p2.::((name, t, c))
          case _ => ctx.declaration(x).type_specifier().getChild(0).getText match {
            case "enum" =>
              val t = TypeRep.EnumRep
              val p = enumP(ctx.declaration(x).type_specifier().enum_type_spec()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
            case "struct" =>
              val t = TypeRep.StructRep
              val p = structP(ctx.declaration(x).type_specifier().struct_type_spec().struct_body()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
            case "union" =>
              val t = TypeRep.UnionRep
              val p = unionP(ctx.declaration().type_specifier(x).union_type_spec().union_body()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
          }
        }
      }
    }
    p2
  }*/

}
