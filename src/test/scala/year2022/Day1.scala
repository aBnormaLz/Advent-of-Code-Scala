package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.SeqOps
import util.Task

class Day1 extends Task(2022, 1) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    input.split("")
      .map(_.map(_.toInt).sum)
      .max
  }

  def part2(input: Seq[String]): Int = {
    input.split("")
      .map(_.map(_.toInt).sum)
      .sorted
      .takeRight(3)
      .sum
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 24000
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 71124
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 45000
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 204639
    }
  }
}
