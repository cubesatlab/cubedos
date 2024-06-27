package edu.vermontstate.mercury

import TypeRep.ComponentRep

/**
 * The interface to symbol tables.
 */
trait SymbolTable {
  def addStructuredName(name: String, typeRep: TypeRep.Rep, value: String): Unit
  def addSTypes(name: String, p: ComponentRep): Unit
  def addTypeName(name: String, typeRep: TypeRep.Rep, value: String): Unit
  def getStructuredTypeNames: Iterable[String]
  def getSType(name: String): Iterable[String]
  def getST(name: String, subName: String): String
  def getTypeNames: Iterable[String]
  def getStructuredType(name: String): TypeRep.Rep
  def getTypeRepresentation(name: String): TypeRep.Rep
  def getTypeValue(name: String): String
  def getMStructs: List[String]
  def getStructuredTypeParent(name: String, subName: String): String
  def getStructuredConstraint(name: String): String
  def getArraySize(name: String): String
  def getArraySSize(name: String, id: String): String
  def getArrayType(name: String): TypeRep.Rep
  def getArraySType(name: String, id: String): String
}

/**
 * Defines several exception classes used by all symbol table classes.
 */
object SymbolTable {
  class SymbolTableException(message: String) extends Exception(message)
  class UnknownObjectNameException(message: String) extends SymbolTableException(message)
  class UnknownTypeNameException(message: String) extends SymbolTableException(message)
  class DuplicateObjectNameException(message: String) extends SymbolTableException(message)
  class DuplicateTypeNameException(message: String) extends SymbolTableException(message)
  class ConflictingNameException(message: String) extends SymbolTableException(message)
}
