package year2022

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.Ops.IntCoord
import util.{Printer, Task}

import java.lang.Math.abs
import scala.collection.mutable.ListBuffer

class Day9 extends Task(2022, 9) with AnyWordSpecLike with Matchers {
  var fieldFrom: IntCoord = (-1, -1)
  var fieldTo: IntCoord   = (+1, +1)
  var stepsTotal: Int     = 0

  def modifyMap(headPos: IntCoord): Unit = {
    if (headPos._1 <= fieldFrom._1) {
      fieldFrom = fieldFrom |+| (-1, 0)
    }
    if (fieldTo._1 <= headPos._1) {
      fieldTo = fieldTo |+| (+1, 0)
    }
    if (headPos._2 <= fieldFrom._2) {
      fieldFrom = fieldFrom |+| (0, -1)
    }
    if (fieldTo._2 <= headPos._2) {
      fieldTo = fieldTo |+| (0, +1)
    }
  }

  def mapToPrettyString(rope: Seq[IntCoord], tailPath: Set[IntCoord]): String = {
    (for (y <- fieldFrom._2 to fieldTo._2) yield {
      (for (x <- fieldFrom._1 to fieldTo._1) yield {
        if (rope.size == 2) {
          if (rope.head == (x, y)) {
            "H"
          } else if (rope.last == (x, y)) {
            "T"
          } else if (tailPath.contains((x, y))) {
            "#"
          } else {
            "."
          }
        } else {
          if (rope.contains((x, y))) {
            "X"
          } else if (tailPath.contains((x, y))) {
            "#"
          } else {
            "."
          }
        }
      }).mkString("")
    }).reverse.mkString("\n")
  }

  def tailPathToPrettyString(tailPath: Set[IntCoord]): String = {
    (for (y <- fieldFrom._2 to fieldTo._2) yield {
      (for (x <- fieldFrom._1 to fieldTo._1) yield {
        if (tailPath.contains((x, y))) {
          "#"
        } else {
          "."
        }
      }).mkString("")
    }).reverse.mkString("\n")
  }

  def calculateNewRopePart(headPos: IntCoord, tailPos: IntCoord): IntCoord = {
    if (1 < abs(headPos._1 - tailPos._1) || 1 < Math.abs(headPos._2 - tailPos._2)) {
      if (headPos._1 == tailPos._1) {
        if (tailPos._2 < headPos._2) {
          tailPos |+| (0, +1)
        } else {
          tailPos |+| (0, -1)
        }
      } else if (headPos._2 == tailPos._2) {
        if (tailPos._1 < headPos._1) {
          tailPos |+| (+1, 0)
        } else {
          tailPos |+| (-1, 0)
        }
      } else {
        if (tailPos._1 < headPos._1 && tailPos._2 < headPos._2) {
          tailPos |+| (+1, +1)
        } else if (tailPos._1 > headPos._1 && tailPos._2 < headPos._2) {
          tailPos |+| (-1, +1)
        } else if (tailPos._1 < headPos._1) {
          tailPos |+| (+1, -1)
        } else {
          tailPos |+| (-1, -1)
        }
      }
    } else {
      tailPos
    }
  }

  def calculateNewRope(rope: Seq[IntCoord], movement: IntCoord): Seq[IntCoord] = {
    val newRope = ListBuffer(rope.head |+| movement)
    for (i <- 1 until rope.size) yield {
      newRope += calculateNewRopePart(newRope.last, rope(i))
    }
    newRope.toSeq
  }

  def solution(input: Seq[String], ropeLength: Int): Int = {
    fieldFrom = (-1, -1)
    fieldTo = (+1, +1)
    val directions = input
      .map(_.split(" ").toSeq)
      .map(_.checkSizeEquals(2))
      .map(command => command.head * command.last.toInt)
      .flatMap(_.split(""))

    stepsTotal = directions.size

    val initialRope = Seq.fill(ropeLength)((0, 0))

//    printer.printLine(mapToPrettyString(initialRope, Set()))
//    printer.printHardLine()

    var nrOfStep = 0

    val (_, tailPath) = directions.foldLeft((initialRope, Set(initialRope.last))) { case ((rope, tailPath), direction) =>
      nrOfStep = nrOfStep + 1
      val movement = direction match {
        case "U"   => (0, +1)
        case "D"   => (0, -1)
        case "L"   => (-1, 0)
        case "R"   => (+1, 0)
        case other => throw new UnknownError(s"Unknown direction: $other")
      }

      val dirMap = Map("U" -> "UP", "D" -> "DOWN", "L" -> "LEFT", "R" -> "RIGHT")

//      printer.printLine(s"Moving: ${dirMap(direction)}")
//      printer.printLine(s"Remaining steps: ${stepsTotal - nrOfStep}")
      val newRope = calculateNewRope(rope, movement)
      modifyMap(newRope.head)
//      printer.printLine(mapToPrettyString(newRope, tailPath))
//      printer.printHardLine()
      (newRope, tailPath + newRope.last)
    }

    printer.printLine("Tail path")
    printer.printLine(tailPathToPrettyString(tailPath))

    tailPath.size
  }

  "Part 1" should {
    "solve the example 1" in {
      printer = new Printer(true)

      val input = getExample(1)
      solution(input, 2) shouldBe 13
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      solution(input, 2) shouldBe 6209
    }
  }

  "Part 2" should {
    "solve the example 1" in {
      printer = new Printer(true)

      val input = getExample(1)
      solution(input, 10) shouldBe 1
    }

    "solve the example 2" in {
      printer = new Printer(true)

      val input = getExample(2)
      solution(input, 10) shouldBe 36
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      solution(input, 10) shouldBe 2460
    }
  }
}
