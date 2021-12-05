package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Printer, Task}

import scala.math.{abs, max, signum}

class Day5 extends Task(2021, 5) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String])(implicit printer: Printer): Int = {
    val (xMax, yMax)   = getXY(input)
    val dangerousLines = getDangerousLines(input)

    printDangerousLines(dangerousLines, xMax, yMax)

    val dangerLevels = accumulateDangerousFields(dangerousLines.flatten)

    printDangerLevels(dangerLevels, xMax, yMax)

    dangerLevels
      .count(_._2 > 1)
  }

  def part2(input: Seq[String])(implicit printer: Printer): Int = {
    val (xMax, yMax)   = getXY(input)
    val dangerousLines = getDangerousLines(input, withDiagonals = true)
    val dangerLevels   = accumulateDangerousFields(dangerousLines.flatten)

    printDangerLevels(dangerLevels, xMax, yMax)

    dangerLevels
      .count(_._2 > 1)
  }

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

  "Part 1" should {
    "solve the example" in {
      implicit val printer: Printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 5
    }

    "solve the task" in {
      implicit val printer: Printer = new Printer(false)

      val input = getTask()
      part1(input) shouldBe 3990
    }
  }

  "Part 2" should {
    "solve the example" in {
      implicit val printer: Printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 12
    }

    "solve the task" in {
      implicit val printer: Printer = new Printer(false)

      val input = getTask()
      part2(input) shouldBe 21305
    }
  }
}
