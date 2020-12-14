package year2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{InputReader, Task}

import scala.annotation.tailrec

class Day5 extends Task(2020, 5) with AnyWordSpecLike with Matchers {
  def calculateRow(code: String): Int = {
    val rowInfo = code.dropRight(3)

    @tailrec
    def rec(charToCheck: Int, acc: Int): Int = {
      if (charToCheck >= 7) {
        acc
      } else {
        if (rowInfo(charToCheck) == 'B') {
          rec(charToCheck + 1, acc + Math.pow(2, 6 - charToCheck).asInstanceOf[Int])
        } else {
          rec(charToCheck + 1, acc)
        }
      }
    }

    rec(0, 0)
  }

  def calculateColumn(code: String): Int = {
    val columnInfo = code.substring(7)

    @tailrec
    def rec(charToCheck: Int, acc: Int): Int = {
      if (charToCheck >= 3) {
        acc
      } else {
        if (columnInfo(charToCheck) == 'R') {
          rec(charToCheck + 1, acc + Math.pow(2, 2 - charToCheck).asInstanceOf[Int])
        } else {
          rec(charToCheck + 1, acc)
        }
      }
    }

    rec(0, 0)
  }

  def calculateSeatId(code: String): Int = {
    calculateRow(code) * 8 + calculateColumn(code)
  }

  def part1(input: Seq[String]): Int = {
    input
      .map(calculateSeatId)
      .max
  }

  "Part 1" should {
    "solve the example" in {
      calculateRow("BFFFBBFRRR") shouldBe 70
      calculateRow("FFFBBBFRRR") shouldBe 14
      calculateRow("BBFFBBFRLL") shouldBe 102

      calculateColumn("BFFFBBFRRR") shouldBe 7
      calculateColumn("FFFBBBFRRR") shouldBe 7
      calculateColumn("BBFFBBFRLL") shouldBe 4

      calculateSeatId("BFFFBBFRRR") shouldBe 567
      calculateSeatId("FFFBBBFRRR") shouldBe 119
      calculateSeatId("BBFFBBFRLL") shouldBe 820
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part1(input) shouldBe 959
    }
  }
}
