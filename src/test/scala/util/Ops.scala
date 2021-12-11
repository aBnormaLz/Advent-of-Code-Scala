package util

import cats.implicits._

object Ops {
  type IntCoord       = (Int, Int)
  type IntCoordValues = Seq[Seq[Int]]

  implicit class IntCoordOps(coord: IntCoord) {
    def up(): IntCoord    = coord |-| (0, 1)
    def down(): IntCoord  = coord |+| (0, 1)
    def left(): IntCoord  = coord |-| (1, 0)
    def right(): IntCoord = coord |+| (1, 0)
  }

  implicit class IntCoordValuesOps(coordValues: IntCoordValues) {
    def isDefinedAt(coord: IntCoord): Boolean = {
      coordValues.lift(coord._1).flatMap(_.lift(coord._2)).isDefined
    }

    def at(coord: IntCoord): Int = {
      coordValues(coord._1)(coord._2)
    }
  }
}
