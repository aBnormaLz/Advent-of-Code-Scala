package year2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{InputReader, Task}

class Day2 extends Task(2020, 2) with AnyWordSpecLike with Matchers {
  case class PasswordDescriptor(
      from: Int,
      to: Int,
      character: String,
      password: String,
  ) {
    def getCorrectnessPart1: Int = {
      val replacedLength = password.replace(character, "").length
      val difference     = password.length - replacedLength

      if (from <= difference && difference <= to) {
        1
      } else {
        0
      }
    }

    def getCorrectnessPart2: Int = {
      val firstChar  = password(from - 1).toString
      val secondChar = password(to - 1).toString

      if (firstChar == character ^ secondChar == character) {
        1
      } else {
        0
      }
    }
  }

  private def toPasswordDescriptor(line: String) = {
    val split1   = line.split(": ")
    val password = split1(1)

    val split2    = split1.head.split(" ")
    val character = split2(1)

    val split3 = split2.head.split("-")

    val from = split3.head.toInt
    val to   = split3(1).toInt

    PasswordDescriptor(
      from,
      to,
      character,
      password,
    )
  }

  def part1(input: Seq[String]): Int = {
    input
      .map(toPasswordDescriptor)
      .map(_.getCorrectnessPart1)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input
      .map(toPasswordDescriptor)
      .map(_.getCorrectnessPart2)
      .sum
  }

  "Part 1" should {
    "solve the example" in {
      toPasswordDescriptor("1-3 a: abcde").getCorrectnessPart1 == 1 shouldBe true
      toPasswordDescriptor("1-3 b: cdefg").getCorrectnessPart1 == 1 shouldBe false
      toPasswordDescriptor("2-9 c: ccccccccc").getCorrectnessPart1 == 1 shouldBe true
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part1(input) shouldBe 524
    }
  }

  "Part 2" should {
    "solve the example" in {
      toPasswordDescriptor("1-3 a: abcde").getCorrectnessPart2 == 1 shouldBe true
      toPasswordDescriptor("1-3 b: cdefg").getCorrectnessPart2 == 1 shouldBe false
      toPasswordDescriptor("2-9 c: ccccccccc").getCorrectnessPart2 == 1 shouldBe false
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part2(input) shouldBe 485
    }
  }
}
