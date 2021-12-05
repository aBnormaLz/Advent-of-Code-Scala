package year2021.day5

import util.Printer

import scala.math.max

object Day5Util {
  def getXY(input: Seq[String])(implicit printer: Printer): (Int, Int) = {
    printer.printLine("====================")
    val (xMax, yMax) = input
      .foldLeft((0, 0)) {
        case ((xMax, yMax), next) =>
          next match {
            case s"$x1s,$y1s -> $x2s,$y2s" =>
              val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
              (max(max(x1, x2), xMax), max(max(y1, y2), yMax))
          }
      }
    printer.printLine(s"Size of the field is $xMax x $yMax")
    printer.printLine("====================")
    (xMax, yMax)
  }

  def getDangerousFields(input: Seq[String], withDiagonals: Boolean = false)(implicit printer: Printer): Seq[(Int, Int)] = {
    printer.printLine("====================")
    val ret = input.flatMap(line => {
      line match {
        case s"$x1s,$y1s -> $x2s,$y2s" =>
          val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
          val dangerousFields = {
            if (x1 == x2) {
              for (y <- orderedRangeOf(y1, y2)) yield {
                (x1, y)
              }
            } else if (y1 == y2) {
              for (x <- orderedRangeOf(x1, x2)) yield {
                (x, y1)
              }
            } else if (withDiagonals && x1 - x2 == y2 - y1) {
              if (x1 < x2) {
                // 5,5 -> 8,2
                for (i <- 0 to (x2 - x1)) yield {
                  (x1 + i, y1 - i)
                }
              } else {
                // 8,0 -> 0,8
                for (i <- 0 to (x1 - x2)) yield {
                  (x1 - i, y1 + i)
                }
              }
            } else if (withDiagonals && x1 - x2 == y1 - y2) {
              if (x1 < x2) {
                // 0,0 -> 8,8
                for (i <- 0 to (x2 - x1)) yield {
                  (x1 + i, y1 + i)
                }
              } else {
                // 6,4 -> 2,0
                for (i <- 0 to (x1 - x2)) yield {
                  (x1 - i, y1 - i)
                }
              }
            } else {
              Seq.empty[(Int, Int)]
            }
          }
          if (dangerousFields.nonEmpty) {
            printer.printLine(s"Parsing $line")
            printer.printLine(s"Dangerous fields: $dangerousFields")
            printer.printLine("--------------------")
          }
          dangerousFields
      }
    })
    printer.printLine("====================")
    ret
  }

  def orderedRangeOf(y1: Int, y2: Int): Seq[Int] = {
    if (y1 > y2) y2 to y1 else y1 to y2
  }

  def accumulateDangerousFields(dangerousFields: Seq[(Int, Int)])(implicit printer: Printer): Map[(Int, Int), Int] = {
    printer.printLine("====================")
    val accumulated = dangerousFields
      .groupBy { case (x, y) => (x, y) }
      .view
      .mapValues(_.length)
      .toMap
    printer.printLine(s"Accumulated dangers are: $accumulated")
    printer.printLine("====================")
    accumulated
  }

  def printDangerLevels(dangerLevels: Map[(Int, Int), Int], xMax: Int, yMax: Int)(implicit printer: Printer): Unit = {
    for (y <- 0 to yMax) yield {
      val fieldLine = (for (x <- 0 to xMax) yield {
        dangerLevels.getOrElse((x, y), ".").toString
      }).mkString
      printer.printLine(fieldLine)
    }
  }
}
