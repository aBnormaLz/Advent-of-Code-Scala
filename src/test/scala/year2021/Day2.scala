package year2021

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day2 extends Task(2021, 2) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    val (horizontal, depth) = input
      .foldLeft(0, 0) {
        case ((horizontal, depth), next) =>
          next match {
            case s"forward $value" =>
              (horizontal, depth) |+| (value.toInt, 0)
            case s"up $value"      =>
              (horizontal, depth) |-| (0, value.toInt)
            case s"down $value"    =>
              (horizontal, depth) |+| (0, value.toInt)
          }
      }
    horizontal * depth
  }

  def part2(input: Seq[String]): Int = {
    val (horizontal, depth, _) = input
      .foldLeft(0, 0, 0) {
        case ((horizontal, depth, aim), next) =>
          next match {
            case s"forward $value" =>
              (horizontal, depth, aim) |+| (value.toInt, aim * value.toInt, 0)
            case s"up $value"      =>
              (horizontal, depth, aim) |-| (0, 0, value.toInt)
            case s"down $value"    =>
              (horizontal, depth, aim) |+| (0, 0, value.toInt)
          }
      }
    horizontal * depth
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 150
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 1804520
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 900
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 1971095320
    }
  }
}
