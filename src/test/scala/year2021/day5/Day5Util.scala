package year2021.day5

import util.Printer

import scala.math.{abs, max, signum}

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

  def getDangerousLines(input: Seq[String], withDiagonals: Boolean = false)(implicit printer: Printer): Seq[Seq[(Int, Int)]] = {
    printer.printLine("====================")
    val ret = input.map {
      case line @ s"$x1s,$y1s -> $x2s,$y2s" =>
        printer.printLine(s"Parsing $line")
        val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
        val dangerousFields = {
          if (x1 == x2 || y1 == y2 || (withDiagonals && (x1 - x2 == y2 - y1 || x1 - x2 == y1 - y2))) {
            val lengthOfLine = max(abs(x1 - x2) + 1, abs(y1 - y2) + 1)
            printer.printLine(s"lengthOfLine: $lengthOfLine")

            for (i <- 0 until lengthOfLine) yield {
              val x = x1 + signum(x2 - x1) * i
              val y = y1 + signum(y2 - y1) * i
              printer.printLine(s"Adding: $x, $y")
              (x, y)
            }
          } else {
            Seq.empty[(Int, Int)]
          }
        }
        if (dangerousFields.nonEmpty) {
          printer.printLine(s"Dangerous fields: $dangerousFields")
        } else {
          printer.printLine("Skipping line...")
        }
        printer.printLine("--------------------")
        dangerousFields
    }
    printer.printLine("====================")
    ret.filter(_.nonEmpty)
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
}
