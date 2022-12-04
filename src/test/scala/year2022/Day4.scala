package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day4 extends Task(2022, 4) with AnyWordSpecLike with Matchers {
  case class ElfPairs(from1: Int, to1: Int, from2: Int, to2: Int) {
    def isFullIntersection: Boolean = {
      (from1 <= from2 && to2 <= to1) || (from2 <= from1 && to1 <= to2)
    }

    def doesOverlap: Boolean = {
      isFullIntersection || (from1 <= from2 && from2 <= to1) || (from1 <= to2 && to2 <= to1)
    }

    def doesNotOverlap: Boolean = {
      to2 < from1 || to1 < from2
    }
  }

  object ElfPairs {
    def apply(line: String): ElfPairs = {
      line match {
        case s"$f1-$t1,$f2-$t2" => this(f1.toInt, t1.toInt, f2.toInt, t2.toInt)
        case other              => throw new UnsupportedOperationException(s"Unknown format: $other")
      }
    }
  }

  def part1(input: Seq[String]): Int = {
    input
      .map(ElfPairs.apply)
      .count(_.isFullIntersection)
  }

  def part2(input: Seq[String]): Int = {
    input
      .map(ElfPairs.apply)
      .count(_.doesOverlap)
  }

  // based on https://youtu.be/d4s7Tuo2L3w?t=382
  def part2try2(input: Seq[String]): Int = {
    input
      .map(ElfPairs.apply)
      .count(!_.doesNotOverlap)
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 2
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 500
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 4
      part2try2(input) shouldBe 4
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 815
      part2try2(input) shouldBe 815
    }
  }
}
