package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Ops.{IntCoord, IntCoordOps, MatrixOps}
import util.{Printer, Task}

import scala.annotation.tailrec
import scala.math.min

class Day15 extends Task(2021, 15) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Int = {
    val map   = input.map(_.split("").map(_.toInt).toSeq)
    var risks = Seq.fill(map.length)(Seq.fill(map.head.length)(Int.MaxValue))
    risks = risks.updatedAt((0, 0), 0)

    val neighbourMappers: Seq[IntCoord => IntCoord] = Seq(
      _.up(),
      _.down(),
      _.left(),
      _.right(),
    )

    @tailrec
    def calculateRisks(queue: Set[IntCoord]): Int = {
      queue.toList match {
        case Nil           => risks.last.last
        case field :: tail =>
          val neighboursToEnqueue = neighbourMappers
            .map(_.apply(field))
            .filter(neighbour => map.isDefinedAt(neighbour))
            .filter(neighbour => risks.at(field) + map.at(neighbour) < risks.at(neighbour))
            .map(neighbour => {
              risks = risks.updatedAt(neighbour, risks.at(field) + map.at(neighbour))
              neighbour
            })

          calculateRisks(tail.toSet ++ neighboursToEnqueue.toSet)

      }
    }

    val lastRisk = calculateRisks(Set((0, 0)))

    val pad = risks.map(_.max).max.toString.length + 1
    risks.printWithValuesPadded(pad)

    lastRisk
  }

  def part1Nop(input: Seq[String]): Int = {
    val map   = input.map(_.split("").map(_.toInt).toSeq)
    var risks = Seq.fill(map.length)(Seq.fill(map.head.length)(0))

    for (x <- 1 until map.length) {
      val field = (x, 0)
      risks = risks.updatedAt(field, risks.at(field.up()) + map.at(field))
    }

    for (y <- 1 until map.head.length) {
      val field = (0, y)
      risks = risks.updatedAt(field, risks.at(field.left()) + map.at(field))
    }

    for (x <- 1 until map.length) {
      for (y <- 1 until map.head.length) {
        val field          = (x, y)
        val minRiskToField = min(risks.at(field.up()), risks.at(field.left()))
        risks = risks.updatedAt(field, minRiskToField + map.at(field))
      }
    }

    val pad = risks.map(_.max).max.toString.length + 1
    risks.printWithValuesPadded(pad)

    risks.last.last
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 40
    }

    // 5 min runtime :/
//    "solve the task" in {
//      printer = new Printer(true)
//
//      val input = getTask()
//      part1(input) shouldBe 415
//    }
  }
}
