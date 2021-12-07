package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._

class Day7 extends Task(2021, 7) with AnyWordSpecLike with Matchers {
  var printer: Printer = _

  def part1(input: Seq[String]): Int = {
    val positions = input.head.split(",").map(_.toInt)

    (positions.min to positions.max).map(gatherPos => {
      positions.map(pos => {
        Math.abs(gatherPos - pos)
      }).sum
    }).min
  }

  def part2(input: Seq[String]): Int = {
    val positions = input.head.split(",").map(_.toInt)

    (positions.min to positions.max).map(gatherPos => {
      positions.map(pos => {
        val n = Math.abs(gatherPos - pos)
        (n * (n + 1)) / 2
      }).sum
    }).min
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 37
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part1(input) shouldBe 333755
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 168
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 94017638
    }
  }
}
