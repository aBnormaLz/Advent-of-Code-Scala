package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._
import year2021.day13.Util._

class Day13 extends Task(2021, 13) with AnyWordSpecLike with Matchers {
  def solution(input: Seq[String], onlyFirstFold: Boolean = true): Paper = {
    val separatorIndex = input.indexOf("")
    val dotInputs      = input.slice(0, separatorIndex)
    val foldInputs     =
      if (onlyFirstFold) {
        Seq(input.slice(separatorIndex + 1, input.length).head)
      } else {
        input.slice(separatorIndex + 1, input.length)
      }

    val (paper, initElapsed) = ElapsedTimeUtil.measureElapsedTimeInMillis(
      Paper(dotInputs),
    )

    printElapsedTimeInMillis(initElapsed, "Initialization")

    val foldAxes = foldInputs.map {
      case s"fold along $axis=$_" => axis
    }

    val finalPaper = foldAxes.foldLeft(paper) {
      case (paper, axis) =>
        val (nextPaper, foldElapsed) = ElapsedTimeUtil.measureElapsedTimeInMillis(
          paper.foldOn(axis),
        )
        printElapsedTimeInMillis(foldElapsed, "Fold")
        nextPaper
    }

    finalPaper
  }

  def printElapsedTimeInMillis(elapsed: Long, qualifier: String): Unit = {
    printer.printLine(s"${TimeUtil.formatMillis(elapsed)} - $qualifier")
    printer.printLine()
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      val paper = solution(input)
      paper.dotCount() shouldBe 17
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      val paper = solution(input)
      paper.dotCount() shouldBe 610
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      val paper = solution(input, onlyFirstFold = false)

      paper.toPrettyString() shouldBe """
          |#####
          |#...#
          |#...#
          |#...#
          |#####
          |.....
          |.....""".stripMargin.tail
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      val paper = solution(input, onlyFirstFold = false)

      paper.print()

      paper.toPrettyString() shouldBe """
          |###..####.####...##.#..#.###..####.####.
          |#..#....#.#.......#.#..#.#..#.#.......#.
          |#..#...#..###.....#.####.#..#.###....#..
          |###...#...#.......#.#..#.###..#.....#...
          |#....#....#....#..#.#..#.#.#..#....#....
          |#....####.#.....##..#..#.#..#.#....####.""".stripMargin.tail
    }
  }
}
