package util.enumeration

object EmptyElementSettings extends Enum {
  type EmptyElementSettings = Value

  val dropEmptyElement: EmptyElementSettings = Value
  val keepEmptyElement: EmptyElementSettings = Value
}
