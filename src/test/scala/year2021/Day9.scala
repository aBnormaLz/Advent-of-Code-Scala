package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.IntCoordValuesOps
import util._
import year2021.day9.Util._

import scala.annotation.tailrec

class Day9 extends Task(2021, 9) with AnyWordSpecLike with Matchers {
  implicit var printer: Printer = _

  def part1(input: Seq[String]): Int = {
    val points = input
      .map(_.toList.map(_.asDigit))
      .transpose

    points
      .findLowPoints()
      .map(point => points.at(point) + 1)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    val points = input
      .map(_.toList.map(_.asDigit))
      .transpose

    val topBasins = points
      .findLowPoints()
      .map(lowPoint => expandBasin(points, lowPoint))
      .sortBy(_.size)
      .reverse
      .take(3)

    topBasins.foreach(basin => {
      printer.printLine(s"Basin size: ${basin.size}")
      printer.printLine()
      basin.print(points)
      printer.printSoftLine()
    })

    topBasins.map(_.size).product
  }

  def expandBasin(pointValues: PointValues, lowPoint: Point): Basin = {
    expandBasin(pointValues, Set(lowPoint), Set())
  }

  @tailrec
  final def expandBasin(pointValues: PointValues, toVisit: Set[Point], basin: Basin): Basin = {
    if (toVisit.isEmpty) {
      basin
    } else {
      val actualPoint = toVisit.head
      val neighbours  = pointValues
        .neighboursOf(actualPoint)
        .filter(point => pointValues.at(point) < 9)

      val nextToVisit = toVisit.tail ++ (neighbours -- basin -- toVisit)
      val nextBasin   = basin + actualPoint

      expandBasin(pointValues, nextToVisit, nextBasin)
    }
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 15
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part1(input) shouldBe 575
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 1134
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 1019700
    }
  }
}
