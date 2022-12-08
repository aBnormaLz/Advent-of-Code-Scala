package util.enumeration

object MatchSettings extends Enum {
  type MatchSettings = Value

  val dropMatch: MatchSettings      = Value
  val keepMatchRight: MatchSettings = Value
  val keepMatchLeft: MatchSettings  = Value
}
