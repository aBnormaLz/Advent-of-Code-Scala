package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

import scala.annotation.tailrec

class Day3 extends Task(2021, 3) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    val transposedSum = input
      .map(_.toList)
      .transpose
      .map(_.map(_.asDigit).sum)

    val gammaBits = transposedSum
      .map(sum => if (sum > input.length / 2) "1" else "0")
      .mkString

    val epsilonBits = transposedSum
      .map(sum => if (sum < input.length / 2) "1" else "0")
      .mkString

    val gamma   = Integer.parseInt(gammaBits, 2)
    val epsilon = Integer.parseInt(epsilonBits, 2)

    gamma * epsilon
  }

  def part2(input: Seq[String]): Int = {
    val oxygenRatingBits = findRating(input, Ordering.Double.TotalOrdering.gteq)
    val co2RatingBits    = findRating(input, Ordering.Double.TotalOrdering.lt)

    val oxygenRating = Integer.parseInt(oxygenRatingBits, 2)
    val co2Rating    = Integer.parseInt(co2RatingBits, 2)

    oxygenRating * co2Rating
  }

  def findRating(input: Seq[String], comparator: (Double, Double) => Boolean): String = {
    findRating(input, getFilterAtPosition(input, 0, comparator), 0, comparator)
  }

  @tailrec
  final def findRating(input: Seq[String], filter: Char, position: Int, comparator: (Double, Double) => Boolean): String = {
    val filtered = input
      .filter(_.charAt(position) == filter)

    filtered match {
      case Seq(elem) => elem
      case _         =>
        val newPosition = position + 1
        val newFilter   = getFilterAtPosition(filtered, newPosition, comparator)
        findRating(filtered, newFilter, newPosition, comparator)
    }
  }

  def getFilterAtPosition(input: Seq[String], position: Int, comparator: (Double, Double) => Boolean): Char = {
    val numberOfOnes = input
      .map(_.charAt(position).asDigit)
      .sum

    if (comparator(numberOfOnes, input.length.toDouble / 2)) '1' else '0'
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 198
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 1540244
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 230
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 4203981
    }
  }
}
