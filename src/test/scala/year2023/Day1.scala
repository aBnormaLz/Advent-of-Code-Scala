package year2023

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.{Printer, Task}

import scala.annotation.tailrec

class Day1 extends Task(2023, 1) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    input
      .map("\\d".r.findAllIn)
      .map(_.toSeq)
      .map(n => n.head + n.last)
      .map(_.toInt)
      .sum
  }

  val replacements: Map[String, String] = Map(
    "one"   -> "1",
    "1"     -> "1",
    "two"   -> "2",
    "2"     -> "2",
    "three" -> "3",
    "3"     -> "3",
    "four"  -> "4",
    "4"     -> "4",
    "five"  -> "5",
    "5"     -> "5",
    "six"   -> "6",
    "6"     -> "6",
    "seven" -> "7",
    "7"     -> "7",
    "eight" -> "8",
    "8"     -> "8",
    "nine"  -> "9",
    "9"     -> "9",
  )

  def part2(input: Seq[String]): Int = {
    input
      .map(line => extractFirstDigit(line) + extractLastDigit(line))
      .map(_.toInt)
      .sum
  }

  def extractFirstDigit(line: String): String = {
    extract(line, (line, key) => line.startsWith(key), _.drop(1))
  }

  def extractLastDigit(line: String): String = {
    extract(line, (line, key) => line.endsWith(key), _.dropRight(1))
  }

  @tailrec
  final def extract(line: String, prediction: (String, String) => Boolean, modifier: String => String): String = {
    val digit = replacements.keys.flatMap(key => {
      if (prediction(line, key)) {
        Some(replacements(key))
      } else {
        None
      }
    })

    if (digit.nonEmpty) {
      digit.checkSizeEquals(1)
      digit.head
    } else {
      extract(modifier(line), prediction, modifier)
    }
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample(1)
      part1(input) shouldBe 142
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 54951
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample(2)
      part2(input) shouldBe 281
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 55218
    }
  }
}
