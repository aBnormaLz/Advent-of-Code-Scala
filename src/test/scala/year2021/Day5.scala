package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Logger, Task}
import year2021.day5.Day5Util._

class Day5 extends Task(2021, 5) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    implicit val log: Logger = new Logger(false)

    val dangerousFields: Seq[(Int, Int)] = getDangerousFields(input)
    val dangerLevels                     = accumulateDangerousFields(dangerousFields)

    dangerLevels
      .count(_._2 > 1)
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 5
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 3990
    }
  }
}
