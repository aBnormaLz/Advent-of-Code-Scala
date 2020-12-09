package y2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.InputReader

class Day1 extends AnyWordSpecLike with Matchers {
  def day1part1(input: Seq[String]): Int = {
    val numbers = input.map(_.toInt)

    val products = for {
      a <- numbers.indices
      b <- (a + 1) until numbers.size
    } yield {
      if (numbers(a) + numbers(b) == 2020) {
        Some(numbers(a) * numbers(b))
      } else {
        None
      }
    }

    val filtered = products
      .filter(_.isDefined)
      .map(_.get)

    if (filtered.size > 1) {
      println(s"Filtered size is ${filtered.size} :(")
    }

    filtered.head
  }

  "Part 1" should {
    "solve the example" in {
      val input = InputReader.getExample(2020, 1)
      day1part1(input) shouldBe 514579
    }

    "solve the task" in {
      val input = InputReader.getTask(2020, 1)
      day1part1(input) shouldBe 1019904
    }
  }
}
