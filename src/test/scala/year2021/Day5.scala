package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Printer, Task}

import scala.math.{abs, max, signum}

class Day5 extends Task(2021, 5) with AnyWordSpecLike with Matchers {
  var printer: Printer = _

  def solution(input: Seq[String], withDiagonals: Boolean): Int = {
    val (xMax, yMax)   = getXY(input)
    val dangerousLines = getDangerousLines(input, withDiagonals)
    val dangerLevels   = accumulateDangerousFields(dangerousLines.flatten)

    printDangerLevels(dangerLevels, xMax, yMax)

    dangerLevels
      .count(_._2 > 1)
  }

  def getXY(input: Seq[String]): (Int, Int) = {
    printer.printHardLine()
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
    printer.printHardLine()
    (xMax, yMax)
  }

  def getDangerousLines(input: Seq[String], withDiagonals: Boolean = false): Seq[Seq[(Int, Int)]] = {
    printer.printHardLine()
    val ret = input.map {
      case line @ s"$x1s,$y1s -> $x2s,$y2s" =>
        printer.printLine(s"Parsing $line")
        val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
        val dangerousFields  = {
          val dx = abs(x1 - x2)
          val dy = abs(y1 - y2)
          if (x1 == x2 || y1 == y2 || (withDiagonals && (dx == dy))) {
            val lengthOfLine = max(dx, dy)
            printer.printLine(s"lengthOfLine: $lengthOfLine")

            for (i <- 0 until (lengthOfLine + 1)) yield {
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
        printer.printSoftLine()
        dangerousFields
    }
    printer.printHardLine()
    ret.filter(_.nonEmpty)
  }

  def accumulateDangerousFields(dangerousFields: Seq[(Int, Int)]): Map[(Int, Int), Int] = {
    printer.printHardLine()
    val accumulated = dangerousFields
      .groupBy { case (x, y) => (x, y) }
      .view
      .mapValues(_.length)
      .toMap
    printer.printLine(s"Accumulated dangers are: $accumulated")
    printer.printHardLine()
    accumulated
  }

  def printDangerLevels(dangerLevels: Map[(Int, Int), Int], xMax: Int, yMax: Int): Unit = {
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
      printer = new Printer(true)

      val input = getExample()
      solution(input, withDiagonals = false) shouldBe 5
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      solution(input, withDiagonals = false) shouldBe 3990
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      solution(input, withDiagonals = true) shouldBe 12
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      solution(input, withDiagonals = true) shouldBe 21305
    }
  }
}
