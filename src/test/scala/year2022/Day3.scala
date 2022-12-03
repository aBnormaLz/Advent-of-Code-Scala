package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day3 extends Task(2022, 3) with AnyWordSpecLike with Matchers {
  def toPriority(commonChar: Char): Int = {
    if ('a'.toInt <= commonChar.toInt && commonChar.toInt <= 'z'.toInt) {
      commonChar.toInt - 'a'.toInt + 1
    } else if ('A'.toInt <= commonChar.toInt && commonChar.toInt <= 'Z'.toInt) {
      commonChar.toInt - 'A'.toInt + 1 + 26
    } else {
      throw new UnknownError(s"Char is: $commonChar, we shouldn't be here...")
    }
  }

  def part1(input: Seq[String]): Int = {
    input
      .map(line => line.splitAt(line.length / 2))
      .map(split => split._1.intersect(split._2)(0))
      .map(toPriority)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.sliding(3, 3)
      .map(group =>
        group.head
          .intersect(group(1))
          .intersect(group(2))(0),
      )
      .map(toPriority)
      .sum
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 157
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 8109
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 70
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 2738
    }
  }
}
