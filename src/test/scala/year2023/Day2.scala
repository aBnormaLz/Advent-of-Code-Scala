package year2023

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.Task

class Day2 extends Task(2023, 2) with AnyWordSpecLike with Matchers {
  case class Game(id: Int, draws: Seq[Map[String, Int]])

  def parseGame(line: String): Game = {
    val split = line.split(": ").toSeq.checkSizeEquals(2)

    val id = split.head match {
      case s"Game $id" => id.toInt
      case other       => throw new Exception(s"Illegal game descriptor: '$other'")
    }

    val draws = split.last.split("; ")
      .map(_.split(", "))
      .map(_.map {
        case s"$number $color" => color -> number.toInt
        case other             => throw new Exception(s"Illegal draw descriptor: '$other'")
      })
      .map(_.groupBy(_._1))
      .map(_.view.mapValues(_.toSeq.checkSizeEquals(1).head._2).toMap)
      .toSeq

    Game(id, draws)
  }

  val maxCubes: Map[String, Int] = Map(
    "red"   -> 12,
    "green" -> 13,
    "blue"  -> 14,
  )

  def isValid(game: Game): Boolean = {
    game.draws.forall(draws => {
      draws.map {
        case (color, number) =>
          number <= maxCubes(color)
      }.forall(identity)
    })
  }

  def part1(input: Seq[String]): Int = {
    val games = input.map(parseGame)

    games
      .filter(isValid)
      .map(_.id)
      .sum
  }

  def calculatePower(game: Game): Int = {
    val initialCubes = Map(
      "red"   -> 0,
      "green" -> 0,
      "blue"  -> 0,
    )

    game.draws.fold(initialCubes)((prevMinCubes, draw) => {
      prevMinCubes.map {
        case (color, number) =>
          if (draw.contains(color) && number < draw(color)) {
            color -> draw(color)
          } else {
            color -> number
          }
      }
    }).values.product
  }

  def part2(input: Seq[String]): Int = {
    val games = input.map(parseGame)

    games
      .map(calculatePower)
      .sum
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 8
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 2369
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 2286
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 66363
    }
  }
}
