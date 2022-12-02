package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day2 extends Task(2022, 2) with AnyWordSpecLike with Matchers {
  def rpsMapper(input: String): Int = {
    input match {
      case "A" | "X" => 1
      case "B" | "Y" => 2
      case "C" | "Z" => 3
      case _         => throw new UnsupportedOperationException(s"$input is unknown")
    }
  }

  def calculateScore(picks: Array[Int]): Int = {
    def resultOf(theirPick: Int, myPick: Int) = {
      (theirPick, myPick) match {
        case _ if (theirPick       % 3) == (myPick % 3) => 3
        case _ if ((theirPick + 1) % 3) == (myPick % 3) => 6
        case _ if ((theirPick + 2) % 3) == (myPick % 3) => 0
        case _ => throw new UnknownError(s"theirPick: $theirPick, myPick: $myPick... We shouldn't be here...")
      }
    }

    picks(1) + resultOf(picks(0), picks(1))
  }

  def enemyPickAndMyResponseMapper(strategy: Array[String]): Array[Int] = {
    (rpsMapper(strategy(0)), strategy(1)) match {
      case (theirPick, "X") => Array(theirPick, (theirPick + 1) % 3 + 1)
      case (theirPick, "Y") => Array(theirPick, (theirPick + 2) % 3 + 1)
      case (theirPick, "Z") => Array(theirPick, (theirPick + 3) % 3 + 1)
      case _                => throw new UnknownError(s"theirPick: ${strategy(0)}, shouldWin: ${strategy(1)}... We shouldn't be here...")
    }
  }

  def part1(input: Seq[String]): Int = {
    input.map(_.split(" "))
      .map(_.map(rpsMapper))
      .map(calculateScore)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.map(_.split(" "))
      .map(enemyPickAndMyResponseMapper)
      .map(calculateScore)
      .sum
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 15
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 13682
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 12
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 12881
    }
  }
}
