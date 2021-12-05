package year2021.day5

import util.Printer
import year2021.day5.Day5Util.accumulateDangerousFields

object Day5Printer {
  def printDangerousLines(dangerousLines: Seq[Seq[(Int, Int)]], xMax: Int, yMax: Int)(implicit printer: Printer): Unit = {
    printer.ifEnabled() {
      printer.printLine("====================")
      printer.printLine("Dangerous lines are:")
      dangerousLines.foreach(line => {
        val from = line.head
        val to   = line.last
        printer.printLine(s"${from._1}, ${from._2} -> ${to._1}, ${to._2}")
        printDangerLevels(accumulateDangerousFields(line), xMax, yMax)
      })
      printer.printLine("====================")
    }
  }

  def printDangerLevels(dangerLevels: Map[(Int, Int), Int], xMax: Int, yMax: Int)(implicit printer: Printer): Unit = {
    printer.ifEnabled() {
      for (y <- 0 to yMax) yield {
        val fieldLine = (for (x <- 0 to xMax) yield {
          dangerLevels.getOrElse((x, y), ".").toString
        }).mkString
        printer.printLine(fieldLine)
      }
    }
  }
}
