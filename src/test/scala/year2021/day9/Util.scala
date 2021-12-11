package year2021.day9

import cats.implicits._
import util.Printer

object Util {
  type Point       = (Int, Int)
  type PointValues = Seq[Seq[Int]]
  type Basin       = Set[Point]

  implicit class PointValuesOps(points: PointValues) {
    def definedAt(point: Point): Boolean = {
      points.lift(point._1).flatMap(_.lift(point._2)).isDefined
    }

    def at(point: Point): Int = {
      points(point._1)(point._2)
    }

    def up(point: Point): Int = {
      if (definedAt(point.up())) at(point.up()) else 10
    }

    def down(point: Point): Int = {
      if (definedAt(point.down())) at(point.down()) else 10
    }

    def left(point: Point): Int = {
      if (definedAt(point.left())) at(point.left()) else 10
    }

    def right(point: Point): Int = {
      if (definedAt(point.right())) at(point.right()) else 10
    }

    def findLowPoints(): Seq[Point] = {
      points.zipWithIndex.map {
        case (line, x) =>
          line.zipWithIndex.map {
            case (_, y) =>
              if ((x, y).isLow(points)) {
                Some((x, y))
              } else {
                None
              }
          }
      } flatMap (_.collect {
        case Some(point) => point
      })
    }

    def neighboursOf(point: Point): Set[Point] = {
      Set(
        if (definedAt(point.up())) Some(point.up()) else None,
        if (definedAt(point.down())) Some(point.down()) else None,
        if (definedAt(point.left())) Some(point.left()) else None,
        if (definedAt(point.right())) Some(point.right()) else None,
      ) collect {
        case Some(point) => point
      }
    }
  }

  implicit class BasinOps(basin: Basin) {
    def ranges(): (Point, Point) = {
      (
        (basin.map(_._1).min, basin.map(_._2).min),
        (basin.map(_._1).max, basin.map(_._2).max),
      )
    }

    def print(values: PointValues)(implicit printer: Printer): Unit = {
      printer.ifEnabled() {
        val ((xMin, yMin), (xMax, yMax)) = basin.ranges()
        for (y <- yMin to yMax) yield {
          val fieldLine = (for (x <- xMin to xMax) yield {
            if (basin.contains((x, y))) {
              values(x)(y).toString
            } else {
              " "
            }
          }).mkString
          printer.printLine(fieldLine)
        }
      }
    }
  }

  implicit class PointOps(point: Point) {
    def up(): Point = {
      point |-| (0, 1)
    }

    def down(): Point = {
      point |+| (0, 1)
    }

    def left(): Point = {
      point |-| (1, 0)
    }

    def right(): Point = {
      point |+| (1, 0)
    }

    def isLow(values: PointValues): Boolean = {
      val value = values.at(point)

      value < values.up(point) &&
      value < values.down(point) &&
      value < values.left(point) &&
      value < values.right(point)
    }
  }

  implicit class ValueOps(int: Int) {
    def prettyPoint(): String = {
      if (int > 9) {
        " "
      } else {
        int.toString
      }
    }
  }
}
