package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day3 extends Task(2021, 3) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    val transposedSum = input
      .map(_.toList)
      .transpose
      .map(_.map(_.asDigit).sum)

    val gammaBits = transposedSum
      .map(sum => if (sum > input.length / 2) "1" else "0")
      .mkString

    val epsilonBits = transposedSum
      .map(sum => if (sum < input.length / 2) "1" else "0")
      .mkString

    val gamma   = Integer.parseInt(gammaBits, 2)
    val epsilon = Integer.parseInt(epsilonBits, 2)

    gamma * epsilon
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 198
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 1540244
    }
  }
}
