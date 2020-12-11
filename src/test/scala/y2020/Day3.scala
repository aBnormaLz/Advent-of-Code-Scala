package y2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{InputReader, Task}

class Day3 extends Task(2020, 3) with AnyWordSpecLike with Matchers {
  def isTree(lineNr: Int, line: String): Int = {
    val characterNrToCheck = (lineNr * 3) % line.length
    val characterToCheck   = line(characterNrToCheck)
    if (characterToCheck == '#') {
      1
    } else {
      0
    }
  }

  def part1(input: Seq[String]): Int = {
    val treeCount = for {
      i <- input.indices
    } yield {
      isTree(i, input(i))
    }

    treeCount.sum
  }

  "Part 1" should {
    "solve the example" in {
      val input = InputReader.getExample(year, day)
      part1(input) shouldBe 7
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part1(input) shouldBe 289
    }
  }
}
