package edu.vermontstate.merc

object TypeRep {

  abstract class Rep

  // Placeholder for situations where there is no type.
  case object NoTypeRep extends Rep

  // Unstructured types.
  case class  IDERep(name: String) extends Rep
  case class  IDRep(name: Rep)     extends Rep
  case object BoolRep     extends Rep
  case object DataRep     extends Rep
  case object DoubleRep   extends Rep
  case object FloatRep    extends Rep
  case object HyperRep    extends Rep
  case object IntRep      extends Rep
  case object OpaqueRep   extends Rep
  case object QuadRep     extends Rep
  case object StringRep   extends Rep
  case object TimeRep     extends Rep
  case object TimeSpanRep extends Rep
  case object TypeDefRep  extends Rep
  case object UHyperRep   extends Rep
  case object UIntRep     extends Rep
  case object VoidRep     extends Rep

  // Structured and composite types.
  case class ArrayRep(size: Int, t: Rep) extends Rep
  case class ComponentRep(components: List[(String, Rep, String)]) extends Rep
  case class ConstRep  (name: String, component: String) extends Rep
  case class EnumRep   (name: String, components: ComponentRep) extends Rep
  case class MStructRep(name: String, components: ComponentRep) extends Rep
  case class StructRep (name: String, components: ComponentRep) extends Rep
  case class UnionRep  (name: String, components: ComponentRep) extends Rep
}
