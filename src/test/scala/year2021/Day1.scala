package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day1 extends Task(2021, 1) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    input
      .map(_.toInt)
      .sliding(2)
      .count(pair => pair.head < pair(1))
  }

  def part2(input: Seq[String]): Int = {
    input
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count(pair => pair.head < pair(1))
  }

  def part2try2(input: Seq[String]): Int = {
    input
      .map(_.toInt)
      .sliding(4)
      .count(quadruplet => quadruplet.head < quadruplet(3))
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 7
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 1374
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 5
      part2try2(input) shouldBe 5
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 1418
      part2try2(input) shouldBe 1418
    }
  }
}
