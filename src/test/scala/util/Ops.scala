package util

import cats.Semigroup
import cats.data.Nested
import cats.implicits._

import scala.math.{max, min}

object Ops {
  implicit class StringOps(str: String) {
    def padLeftTo[B >: Char](len: Int, elem: Char): String = {
      str.reverse.padTo(len, elem).reverse
    }

    def trimRight: String = str.replaceAll("\\s+$", "")
  }

  implicit class IntOps(int: Int) {
    def butMax(includingMax: Int): Int = {
      min(int, includingMax)
    }
  }

  type IntCoord = (Int, Int)

  implicit class IntCoordOps(coord: IntCoord) {
    def up(): IntCoord    = coord |-| (1, 0)
    def down(): IntCoord  = coord |+| (1, 0)
    def left(): IntCoord  = coord |-| (0, 1)
    def right(): IntCoord = coord |+| (0, 1)
  }

  implicit class IntCoordsOps(coords: Seq[IntCoord]) {
    def getMaxes(): IntCoord = {
      coords
        .foldLeft((0, 0)) {
          case ((xMax, yMax), next) =>
            next match {
              case (x, y) =>
                (max(x, xMax), max(y, yMax))
            }
        }
    }
  }

  implicit class MatrixOps[T: Semigroup](matrix: Seq[Seq[T]]) {
    def isDefinedAt(coord: IntCoord): Boolean = {
      matrix.lift(coord._1).flatMap(_.lift(coord._2)).isDefined
    }

    def at(coord: IntCoord): T = {
      matrix(coord._1)(coord._2)
    }

    def atOption(coord: IntCoord): Option[T] = {
      if (isDefinedAt(coord))
        matrix(coord._1)(coord._2).some
      else {
        None
      }
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

    def |+|(right: Seq[Seq[T]]): Seq[Seq[T]] = {
      matrix.zipWithIndex.map {
        case (leftLine, i) =>
          leftLine.zip(right(i)).map { case (x, y) => x |+| y }
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

    def toPrettyString(valueMapper: T => String): String = {
      matrix.map(
        _.map(valueMapper).mkString(""),
      ).mkString("\n")
    }

    def printWithValuesPadded(pad: Int)(implicit printer: Printer): Unit = {
      printer.printLine(toPrettyString(
        _.toString.padLeftTo(pad, ' '),
      ))
    }
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def split(splitBy: T): Seq[Seq[T]] = {
      split(_ == splitBy)
    }

    def split(predicate: T => Boolean): Seq[Seq[T]] = {
      seq.foldLeft(Seq.empty[Seq[T]])((prev, act) => {
        act match {
          case elem if predicate(elem) => prev :+ Seq.empty[T]
          case elem if prev.isEmpty    => Seq(Seq(elem))
          case elem                    => prev.init :+ (prev.last :+ elem)
        }
      })
    }
  }
}
