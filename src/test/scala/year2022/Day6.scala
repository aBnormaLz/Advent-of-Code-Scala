package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.Task

class Day6 extends Task(2022, 6) with AnyWordSpecLike with Matchers {
  def indexOfDistinctRegion(input: String, regionSize: Int): Int = {
    input
      .sliding(regionSize)
      .map {
        case quad if quad.toSet.size < regionSize => false
        case _                                    => true
      }.indexOf(true) + regionSize
  }

  "Part 1" should {
    "solve the example" in {
      indexOfDistinctRegion("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4) shouldBe 7
      indexOfDistinctRegion("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) shouldBe 5
      indexOfDistinctRegion("nppdvjthqldpwncqszvftbrmjlhg", 4) shouldBe 6
      indexOfDistinctRegion("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) shouldBe 10
      indexOfDistinctRegion("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) shouldBe 11
    }

    "solve the task" in {
      val input = getTask().checkSizeEquals(1).head
      indexOfDistinctRegion(input, 4) shouldBe 1965
    }
  }

  "Part 2" should {
    "solve the example" in {
      indexOfDistinctRegion("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) shouldBe 19
      indexOfDistinctRegion("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) shouldBe 23
      indexOfDistinctRegion("nppdvjthqldpwncqszvftbrmjlhg", 14) shouldBe 23
      indexOfDistinctRegion("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) shouldBe 29
      indexOfDistinctRegion("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) shouldBe 26
    }

    "solve the task" in {
      val input = getTask().checkSizeEquals(1).head
      indexOfDistinctRegion(input, 14) shouldBe 2773
    }
  }
}
