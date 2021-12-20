package util

import cats.data.Nested
import cats.implicits._

object Ops {
  type IntCoord = (Int, Int)

  implicit class IntCoordOps(coord: IntCoord) {
    def up(): IntCoord    = coord |-| (0, 1)
    def down(): IntCoord  = coord |+| (0, 1)
    def left(): IntCoord  = coord |-| (1, 0)
    def right(): IntCoord = coord |+| (1, 0)
  }

  implicit class MatrixOps[T](matrix: Seq[Seq[T]]) {
    def isDefinedAt(coord: IntCoord): Boolean = {
      matrix.lift(coord._1).flatMap(_.lift(coord._2)).isDefined
    }

    def at(coord: IntCoord): T = {
      matrix(coord._1)(coord._2)
    }

    def updatedAt(coord: IntCoord, f: T => T): Seq[Seq[T]] = {
      if (matrix.isDefinedAt(coord)) {
        matrix.updatedAt(coord, f(matrix.at(coord)))
      } else {
        matrix
      }
    }

    def updatedAt(coord: IntCoord, value: T): Seq[Seq[T]] = {
      if (matrix.isDefinedAt(coord)) {
        matrix.updated(coord._1, matrix(coord._1).updated(coord._2, value))
      } else {
        matrix
      }
    }

    def nestedForeach(innerCallback: T => Unit)(outerCallback: Seq[T] => Unit): Unit = {
      matrix.foreach(inner => {
        inner.foreach(innerCallback)
        outerCallback(inner)
      })
    }

    def innerMap[R](callback: T => R): Seq[Seq[R]] = {
      Nested(matrix).map(callback).value
    }
  }
}
