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

  def day1part1try2(input: Seq[String]): Int = {
    input
      .map(_.toInt)
      .combinations(2)
      .find(_.sum == 2020)
      .map(_.product)
      .get
  }

  def day1part2(input: Seq[String]): Int = {
    val numbers = input.map(_.toInt)

    val products: Seq[Option[Int]] = for {
      a <- numbers.indices
      b <- (a + 1) until numbers.size
      c <- (b + 1) until numbers.size
    } yield {
      if (numbers(a) + numbers(b) + numbers(c) == 2020) {
        Some(numbers(a) * numbers(b) * numbers(c))
      } else {
        None
      }
    }

    val filtered: Seq[Int] = products
      .filter(_.isDefined)
      .map(_.get)

    if (filtered.size > 1) {
      println(s"Filtered size is ${filtered.size} :(")
    }

    filtered.head
  }

  def day1part2try2(input: Seq[String]): Int = {
    input
      .map(_.toInt)
      .combinations(3)
      .find(_.sum == 2020)
      .map(_.product)
      .get
  }

  "Part 1" should {
    "solve the example" in {
      val input = InputReader.getExample(2020, 1)
      day1part1(input) shouldBe 514579
      day1part1try2(input) shouldBe 514579
    }

    "solve the task" in {
      val input = InputReader.getTask(2020, 1)
      day1part1(input) shouldBe 1019904
      day1part1try2(input) shouldBe 1019904
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = InputReader.getExample(2020, 1)
      day1part2(input) shouldBe 241861950
      day1part2try2(input) shouldBe 241861950
    }

    "solve the task" in {
      val input = InputReader.getTask(2020, 1)
      day1part2(input) shouldBe 176647680
      day1part2try2(input) shouldBe 176647680
    }
  }
}
