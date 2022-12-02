package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._
import year2021.day11.Util._

import scala.annotation.tailrec

class Day11 extends Task(2021, 11) with AnyWordSpecLike with Matchers {
  val part1StepsToSimulate = 100

  def part1(input: Seq[String]): Int = {
    val octopiEnergies = parseOctopiEnergies(input)
    val (_, flashes)   = simulateStepsAndAccumulateFlashes(octopiEnergies, part1StepsToSimulate, Seq())
    flashes.size
  }

  def part2(input: Seq[String]): Int = {
    val octopiEnergies = parseOctopiEnergies(input)
    val (_, steps)     = simulateUntilSync(octopiEnergies, 0)
    steps
  }

  def parseOctopiEnergies(input: Seq[String]): OctopiEnergies = {
    input
      .map(_.toList.map(_.asDigit))
      .transpose
  }

  @tailrec
  final def simulateStepsAndAccumulateFlashes(octopiEnergies: OctopiEnergies, remainingSteps: Int, flashes: Seq[Pos]): (OctopiEnergies, Seq[Pos]) = {
    printer.printLine(s"After step: ${part1StepsToSimulate - remainingSteps}")
    printer.printLine()
    octopiEnergies.printEnergies()
    printer.printSoftLine()
    if (remainingSteps < 1) {
      (octopiEnergies, flashes)
    } else {
      val incremented                = octopiEnergies.incrementAllBy(1)
      val (nextEnergies, newFlashes) = simulateFlashes(incremented)
      simulateStepsAndAccumulateFlashes(nextEnergies, remainingSteps - 1, flashes ++ newFlashes)
    }
  }

  @tailrec
  final def simulateUntilSync(octopiEnergies: OctopiEnergies, step: Int): (OctopiEnergies, Int) = {
    printer.printLine(s"After step: $step")
    printer.printLine()
    octopiEnergies.printEnergies()
    printer.printSoftLine()
    if (octopiEnergies.isSynced()) {
      (octopiEnergies, step)
    } else {
      val incremented       = octopiEnergies.incrementAllBy(1)
      val (nextEnergies, _) = simulateFlashes(incremented)
      simulateUntilSync(nextEnergies, step + 1)
    }
  }

  def simulateFlashes(octopiEnergies: OctopiEnergies): (OctopiEnergies, Seq[Pos]) = {
    val toFlash = octopiEnergies.getFlashPoses()
    simulateFlashes(octopiEnergies, toFlash, Set())
  }

  @tailrec
  final def simulateFlashes(octopiEnergies: OctopiEnergies, toFlash: Set[Pos], flashed: Set[Pos]): (OctopiEnergies, Seq[Pos]) = {
    if (toFlash.isEmpty) {
      (octopiEnergies, flashed.toSeq)
    } else {
      val flashing = toFlash.head

      val nextOctopiEnergies = octopiEnergies.flash(flashing, flashed)
      val nextToFlash        = toFlash ++ (nextOctopiEnergies.getFlashPoses() -- toFlash -- flashed) - flashing
      val nextFlashed        = flashed + flashing

      simulateFlashes(nextOctopiEnergies, nextToFlash, nextFlashed)
    }
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 1656
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part1(input) shouldBe 1773
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 195
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 494
    }
  }
}
