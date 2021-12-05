package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Logger, Task}
import year2021.day5.Day5Util._

class Day5 extends Task(2021, 5) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String])(implicit log: Logger): Int = {
    val dangerousFields: Seq[(Int, Int)] = getDangerousFields(input)
    val dangerLevels                     = accumulateDangerousFields(dangerousFields)

    dangerLevels
      .count(_._2 > 1)
  }

  def part2(input: Seq[String])(implicit log: Logger): Int = {
    val (xMax, yMax)                       = getXY(input)
    val dangerousFields: Seq[(Int, Int)]   = getDangerousFields(input, withDiagonals = true)
    val dangerLevels: Map[(Int, Int), Int] = accumulateDangerousFields(dangerousFields)

    printDangerLevels(dangerLevels, xMax, yMax)

    dangerLevels
      .count(_._2 > 1)
  }

  "Part 1" should {
    "solve the example" in {
      implicit val log: Logger = new Logger(true)

      val input = getExample()
      part1(input) shouldBe 5
    }

    "solve the task" in {
      implicit val log: Logger = new Logger(false)

      val input = getTask()
      part1(input) shouldBe 3990
    }
  }

  "Part 2" should {
    "solve the example" in {
      implicit val log: Logger = new Logger(true)

      val input = getExample()
      part2(input) shouldBe 12
    }

    "solve the task" in {
      implicit val log: Logger = new Logger(false)

      val input = getTask()
      part2(input) shouldBe 21305
    }
  }
}
