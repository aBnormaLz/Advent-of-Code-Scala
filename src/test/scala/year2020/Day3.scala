package year2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

import scala.annotation.tailrec

class Day3 extends Task(2020, 3) with AnyWordSpecLike with Matchers {
  def isTree(lineNr: Int, line: String, right: Int): Int = {
    val characterNrToCheck = (lineNr * right) % line.length
    val characterToCheck   = line(characterNrToCheck)
    if (characterToCheck == '#') {
      1
    } else {
      0
    }
  }

  def countTrees(lines: Seq[String], right: Int, down: Int): Long = {
    val linesToVisit = lines.grouped(down).map(_.head).toSeq
    val treeCount    = for {
      i <- linesToVisit.indices
    } yield {
      isTree(i, linesToVisit(i), right)
    }

    treeCount.sum
  }

  def part1(input: Seq[String]): Long = {
    countTrees(input, 3, 1)
  }

  def part2(input: Seq[String]): Long = {
    val val1 = countTrees(input, 1, 1)
    val val2 = countTrees(input, 3, 1)
    val val3 = countTrees(input, 5, 1)
    val val4 = countTrees(input, 7, 1)
    val val5 = countTrees(input, 1, 2)

    val1 * val2 * val3 * val4 * val5
  }

  def countTreesRecursive(input: Seq[String], step: (Int, Int)): Long = {
    @tailrec
    def rec(actual: (Int, Int), acc: Long): Long = {
      import cats.implicits._
      if (actual._2 > input.size - 1) {
        acc
      } else {
        val lineToCheck = input(actual._2 % input.length)
        val charToCheck = lineToCheck.charAt(actual._1 % lineToCheck.length)
        if (charToCheck == '#') {
          rec(actual |+| step, acc + 1)
        } else {
          rec(actual |+| step, acc)
        }
      }
    }

    rec((0, 0), 0)
  }

  def part2recursive(input: Seq[String]): Long = {
    val val1 = countTreesRecursive(input, (1, 1))
    val val2 = countTreesRecursive(input, (3, 1))
    val val3 = countTreesRecursive(input, (5, 1))
    val val4 = countTreesRecursive(input, (7, 1))
    val val5 = countTreesRecursive(input, (1, 2))

    val1 * val2 * val3 * val4 * val5
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 7
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 289
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()

      countTrees(input, 1, 1) shouldBe 2
      countTrees(input, 3, 1) shouldBe 7
      countTrees(input, 5, 1) shouldBe 3
      countTrees(input, 7, 1) shouldBe 4
      countTrees(input, 1, 2) shouldBe 2

      part2(input) shouldBe 336

      countTreesRecursive(input, (1, 1)) shouldBe 2
      countTreesRecursive(input, (3, 1)) shouldBe 7
      countTreesRecursive(input, (5, 1)) shouldBe 3
      countTreesRecursive(input, (7, 1)) shouldBe 4
      countTreesRecursive(input, (1, 2)) shouldBe 2

      part2recursive(input) shouldBe 336
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 5522401584L
      part2recursive(input) shouldBe 5522401584L
    }
  }
}
