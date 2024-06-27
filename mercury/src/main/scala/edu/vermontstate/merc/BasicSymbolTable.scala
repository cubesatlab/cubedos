package edu.vermontstate.merc

import SymbolTable.*
import TypeRep.ComponentRep

/**
 * A simple implementation of the SymbolTable trait that stores symbols in in-memory maps.
 */
class BasicSymbolTable extends SymbolTable {

  private var structuredTypeMap = Map[String, (TypeRep.Rep, String)]()
  private var sTypes = Map[String, ComponentRep]()
  private var typeMap = Map[String, (TypeRep.Rep, String)]()


  // Adds new structured type.
  def addStructuredName(name: String, typeRep: TypeRep.Rep, value: String): Unit = {
    // TODO: Include in the message the position of the error in the source text.
    if (structuredTypeMap.contains(name))
      throw new DuplicateObjectNameException(s"$name already names an object")

    structuredTypeMap = structuredTypeMap + (name -> ((typeRep, value)))
  }


  // Adds message struct parameters.
  def addSTypes(name: String, p: ComponentRep): Unit = {
    sTypes = sTypes + (name -> p)
  }


  // Adds new unstructured type.
  def addTypeName(name: String, typeRep: TypeRep.Rep, value: String): Unit = {
    // TODO: Include in the message the position of the error in the source text.
    if (typeMap.contains(name))
      throw new DuplicateTypeNameException(s"$name already names a type")

    typeMap = typeMap + (name -> ((typeRep, value)))
  }


  // Returns iterable of all structured type names.
  def getStructuredTypeNames: Iterable[String] = {
    structuredTypeMap.keys
  }


  // Returns iterable of variable names in a message struct.
  def getSType(name: String): Iterable[String] = {
    sTypes(name).components.map(x => x._1)
  }


  // Returns the type of a supplied variable name in a message struct.
  def getST(name: String, subName: String): String = {
    val c = sTypes(name).components.filter(_._1 == subName).map(x => x._2)
    var v = ""
    if (c.nonEmpty) {
      v = c.head.toString
      if (v.contains("(")) {
        if (v.substring(0, v.indexOf("(")).contentEquals("IDRep")) {
          v = v.substring(v.indexOf("(") + 1)
        }
        if (v.contains("(")) {
          v = v.substring(0, v.indexOf("("))
        }
        if (v.contains(")")) {
          v = v.substring(0, v.indexOf(")"))
        }
      }
    }
    v
  }


  // Returns the structured type variable name of a given variable within a message struct.
  def getStructuredTypeParent(name: String, subName: String): String = {
    var d = ""
    val c = sTypes(name).components.filter(_._1 == subName).map(x => x._3)
    if (c.nonEmpty) {
      d = c.head.toString
    }
    d
  }


  // Returns iterable of unstructured type variable names.
  def getTypeNames: Iterable[String] = {
    typeMap.keys
  }


  // Returns the type of a structured type.
  def getStructuredType(name: String): TypeRep.Rep = {
    // TODO: Include in the message the position of the error in the source text.
    if (!structuredTypeMap.contains(name))
      throw new UnknownObjectNameException(s"$name is not the name of an object")

    structuredTypeMap(name)._1
  }


  // Returns the type of an unstructured variable.
  def getTypeRepresentation(name: String): TypeRep.Rep = {
    // TODO: Include in the message the position of the error in the source text.
    if (!typeMap.contains(name))
      return TypeRep.NoTypeRep
    else
      typeMap(name)._1
  }


  // Returns the stored value of a given unstructured type.
  def getTypeValue(name: String): String = {
    if (!typeMap.contains(name))
      throw new UnknownObjectNameException(s"$name is not the name of an object")

    typeMap(name)._2
  }


  // Returns Array inner type.
  def getArrayType(name: String): TypeRep.Rep = {
    if (!typeMap.contains(name)) {
      throw new UnknownObjectNameException(s"$name is not the name of an object")
    }
    else {
      val c1 = typeMap(name)._1.toString.lastIndexOf("(") + 1
      val c2 = typeMap(name)._1.toString.lastIndexOf(")") - 1
      val s1 = typeMap(name)._1.toString.substring(c1, c2)

      if (s1.contentEquals("IntRep")) {
        val v1 = TypeRep.IntRep
        return v1
      }
      else if (s1.contentEquals("UIntRep")) {
        val v1 = TypeRep.UIntRep
        return v1
      }
    }
    TypeRep.NoTypeRep
  }


  // Returns Array inner type from MStruct parameters.
  def getArraySType(name: String, id: String): String = {
    if (!sTypes.contains(name)) {
      throw new UnknownObjectNameException(s"$name is not the name of an object")
    }
    val c = sTypes(name).components.filter(_._1 == id).map(x => x._2)
    var v = ""
    if (c.nonEmpty) {
      v = c.head.toString
      if (v.contains(",")){
        val index1 = v.lastIndexOf('(') + 1
        v = v.substring(index1)
        val index2 = v.indexOf(',') + 1
        v = v.substring(index2)
        val index3 = v.indexOf(")")
        v = v.substring(0, index3)
      }
    }
    if (c.head.toString.contains("StructRep")){
      v = "StructRep"
    }
    else if (c.head.toString.contains("EnumRep")){
      v = "EnumRep"
    }
    v
  }


  // Returns Array size (upper value).
  def getArraySize(name: String): String = {
    if (!typeMap.contains(name))
      throw new UnknownObjectNameException(s"$name is not the name of an object")

    val c1 = typeMap(name)._1.toString.indexOf("(", 0) + 1
    val c2 = typeMap(name).toString().indexOf(",", 0) - 1

    val s1 = typeMap(name)._1.toString.substring(c1, c2)
    s1
  }


  // Returns Array size (upper value).
  def getArraySSize(name: String, id: String): String = {
    if (!sTypes.contains(name))
      throw new UnknownObjectNameException(s"$name is not the name of an object")

    val c = sTypes(name).components.filter(_._1 == id).map(x => x._2)
    var v = ""
    if (c.nonEmpty) {
      v = c.head.toString
      if (v.contains(",")){
        val index1 = v.indexOf(',')
        v = v.substring(0, index1)
        v = v.substring(v.lastIndexOf('(') + 1)
      }
    }
    v
  }


  // Returns a list of all message structs by variable name.
  def getMStructs: List[String] = {
    var MSs = List[String]()
    for (i <- structuredTypeMap) {
      if (getStructuredType(i._1).toString.contains("MStructRep")) {
        MSs = (i._1) :: MSs
      }
    }
    MSs
  }


  // Checks to see if a given name is already present in the map of unstructured types.
  // 0 for No, 1 for Yes.
  def checkTypes(name: String): Int = {
    val n = getTypeNames
    var b = 0
    for (i <- n) {
      if (i == name) {
        b = 1
      }
    }
    b
  }


  // Checks to see if a given name is already present in the map of structured types.
  // 0 for No, 1 for Yes.
  def checkSTypes(name: String): Int = {
    val n = getStructuredTypeNames
    var b = 0
    for (i <- n) {
      if (i == name) {
        b = 1
      }
      val m = getSType(i)
      for (y <- m) {
        if (y == name) {
          b = 1
        }
      }
    }
    b
  }


  // Returns stored value on a structured type.
  def getStructuredConstraint(name: String): String = {
    if (!structuredTypeMap.contains(name))
      throw new UnknownTypeNameException(s"$name is not the name of an type")

    structuredTypeMap(name)._2
  }
}
