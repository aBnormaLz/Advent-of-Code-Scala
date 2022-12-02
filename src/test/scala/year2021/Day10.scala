package year2021

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._

import scala.annotation.tailrec

class Day10 extends Task(2021, 10) with AnyWordSpecLike with Matchers {
  val opens: Seq[Char]  = Seq('(', '[', '{', '<')
  val closes: Seq[Char] = Seq(')', ']', '}', '>')

  implicit class CharOps(char: Char) {
    def closing(opening: Char): Boolean = {
      char match {
        case ')' => '(' == opening
        case ']' => '[' == opening
        case '}' => '{' == opening
        case '>' => '<' == opening
      }
    }
  }

  def part1(input: Seq[String]): Int = {
    input
      .flatMap(findFirstError)
      .map(illegalCharacterToPoint)
      .sum
  }

  def findFirstError(line: String): Option[String] = {
    findFirstError("", line)
  }

  @tailrec
  final def findFirstError(openings: String, remaining: String): Option[String] = {
    if (remaining.isEmpty) {
      None
    } else {
      remaining.head match {
        case char if closes.contains(char) && !char.closing(openings.last) =>
          char.toString.some
        case char if closes.contains(char) && char.closing(openings.last)  =>
          findFirstError(openings.dropRight(1), remaining.tail)
        case char if opens.contains(char)                                  =>
          findFirstError(openings :+ char, remaining.tail)
      }
    }
  }

  def illegalCharacterToPoint(char: String): Int = {
    char match {
      case ")" => 3
      case "]" => 57
      case "}" => 1197
      case ">" => 25137
    }
  }

  def part2(input: Seq[String]): Long = {
    val scores = input
      .map(correctButUnclosed)
      .collect { case Some(s) => s }
      .map(_.reverse)
      .map(openingToClosing)
      .map(_.foldLeft(0L)(calculateNextValue))
      .sorted

    scores(scores.size / 2)
  }

  def correctButUnclosed(line: String): Option[String] = {
    correctButUnclosed("", line)
  }

  @tailrec
  final def correctButUnclosed(openings: String, remaining: String): Option[String] = {
    if (remaining.isEmpty) {
      openings.some
    } else {
      remaining.head match {
        case char if closes.contains(char) && !char.closing(openings.last) =>
          None
        case char if closes.contains(char) && char.closing(openings.last)  =>
          correctButUnclosed(openings.dropRight(1), remaining.tail)
        case char if opens.contains(char)                                  =>
          correctButUnclosed(openings :+ char, remaining.tail)
      }
    }
  }

  def openingToClosing(openings: String): String = {
    openings.map {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }
  }

  def calculateNextValue(value: Long, next: Char): Long = {
    next match {
      case ')' => value * 5 + 1
      case ']' => value * 5 + 2
      case '}' => value * 5 + 3
      case '>' => value * 5 + 4
    }
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 26397
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part1(input) shouldBe 392097
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 288957L
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 4263222782L
    }
  }
}
