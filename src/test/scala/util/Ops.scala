package util

import cats.Semigroup
import cats.data.Nested
import cats.implicits._
import util.enumeration.EmptyElementSettings._
import util.enumeration.MatchSettings._

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

    def upLeft(): IntCoord    = up().left()
    def upRight(): IntCoord   = up().right()
    def downLeft(): IntCoord  = down().left()
    def downRight(): IntCoord = down().right()

    def neighbours(): Seq[IntCoord] = Seq(
      up(),
      down(),
      left(),
      right(),
      upLeft(),
      upRight(),
      downLeft(),
      downRight(),
    )

    def isIn(min: IntCoord, max: IntCoord): Boolean = {
      min._1 <= coord._1 && coord._1 <= max._1 &&
      min._2 <= coord._2 && coord._2 <= max._2
    }
  }

  implicit class IntCoordsOps(coords: Seq[IntCoord]) {
    def getMaxes(): IntCoord = {
      coords
        .foldLeft((Int.MinValue, Int.MinValue)) {
          case ((xMax, yMax), next) =>
            next match {
              case (x, y) =>
                (max(x, xMax), max(y, yMax))
            }
        }
    }

    def getMins(): IntCoord = {
      coords
        .foldLeft((Int.MaxValue, Int.MaxValue)) {
          case ((xMin, yMin), next) =>
            next match {
              case (x, y) =>
                (min(x, xMin), min(y, yMin))
            }
        }
    }

    def neighbours(): Set[IntCoord] = {
      coords.flatMap(_.neighbours()).toSet -- coords
    }

    def prettySorted(): Seq[IntCoord] = {
      coords.sortBy(r => (r._1, r._2))
    }

    def prettyPrint(implicit printer: Printer): Unit = {
      val maxes = getMaxes()
      val mins  = getMins()

      for (y <- mins._2 to maxes._2) {
        for (x <- mins._1 to maxes._1) {
          if (coords.contains((x, y))) {
            printer.print(s"($x, $y), ")
          } else {
            printer.print(s"        ")
          }
        }
        printer.printLine()
      }
      printer.printLine()
      printer.printLine()
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

  case class SplitSettings(
      matchSetting: MatchSettings = dropMatch,
      emptyElementSetting: EmptyElementSettings = dropEmptyElement,
  )

  implicit class SeqOps[T](seq: Seq[T]) {
    def split(elem: T, splitSettings: SplitSettings = SplitSettings()): Seq[Seq[T]] = {
      splitBy(_ == elem, splitSettings)
    }

    def splitBy(predicate: T => Boolean, splitSettings: SplitSettings = SplitSettings()): Seq[Seq[T]] = {
      val matchSetting        = splitSettings.matchSetting
      val emptyElementSetting = splitSettings.emptyElementSetting

      val retVal = seq.foldLeft(Seq.empty[Seq[T]])((prev: Seq[Seq[T]], act) => {
        act match {
          case elem if predicate(elem) =>
            val appendTo = if (prev.isEmpty) Seq(Seq()) else prev
            matchSetting match {
              case `dropMatch`      => appendTo :+ Seq()
              case `keepMatchRight` => appendTo :+ Seq(elem)
              case `keepMatchLeft`  =>
                if (prev.isEmpty) {
                  Seq(Seq(elem), Seq())
                } else {
                  appendTo.updated(prev.size - 1, prev.last :+ elem) :+ Seq()
                }
            }
          case elem if prev.isEmpty    => Seq(Seq(elem))
          case elem                    => prev.init :+ (prev.last :+ elem)
        }
      })

      emptyElementSetting match {
        case `keepEmptyElement` => retVal
        case `dropEmptyElement` => retVal.filter(_.nonEmpty)
      }
    }
  }
}
