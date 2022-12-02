package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._

import scala.annotation.tailrec

class Day14 extends Task(2021, 14) with AnyWordSpecLike with Matchers {
  var stepsToSimulate: Int = _

  def part1(input: Seq[String]): Long = {
    val (template, rules) = parseInput(input)
    val polymer           = buildPolymerBySimulation(template, rules)
    val occurrences       = polymer
      .foldLeft(Map[Char, Long]()) {
        case (occMap, char) =>
          val occ = occMap.getOrElse(char, 0L)
          occMap + (char -> (occ + 1L))
      }
      .toSeq
      .sortBy(_._2)

    printer.printLine(s"Character occurrences")
    occurrences.foreach { case (pair, occ) => printer.printLine(s"$pair -> $occ") }

    occurrences.last._2 - occurrences.head._2
  }

  def buildPolymerBySimulation(template: String, rules: Map[String, String]): String = {
    @tailrec
    def buildPolymerBySimulation(polymer: String, step: Int): String = {
      if (step > stepsToSimulate) {
        printer.printLine(s"After step ${step - 1}: $polymer")
        polymer
      } else {
        val nextPolymer = polymer
          .sliding(2)
          .map(pair => {
            pair.head + rules(pair)
          })
          .mkString + polymer.last
        buildPolymerBySimulation(nextPolymer, step + 1)
      }
    }
    printer.printLine(s"Template: $template")
    buildPolymerBySimulation(template, 1)
  }

  def part2(input: Seq[String]): Long = {
    val (template, rules)   = parseInput(input)
    val polymer             = buildPolymerByMap(template, rules)
    val occsWithoutLastChar = polymer
      .toSeq
      .map { case (pair, occ) => pair.head -> occ }
      .groupBy(_._1)
      .map { case (char, occs) => char -> occs.map(_._2).sum }

    val lastCharOcc = occsWithoutLastChar.getOrElse(template.last, 0L)

    val occurrences =
      (occsWithoutLastChar + (template.last -> (lastCharOcc + 1L)))
        .toSeq
        .sortBy(_._2)

    printer.printLine(s"Character occurrences")
    occurrences.foreach { case (char, occ) => printer.printLine(s"$char -> $occ") }

    occurrences.last._2 - occurrences.head._2
  }

  def buildPolymerByMap(template: String, rules: Map[String, String]): Map[String, Long] = {
    @tailrec
    def buildPolymerByMap(occMap: Map[String, Long], step: Int): Map[String, Long] = {
      if (step > stepsToSimulate) {
        printer.printLine(s"After step ${step - 1}")
        occMap.foreach { case (pair, occ) => printer.printLine(s"$pair -> $occ") }
        printer.printSoftLine()
        occMap
      } else {
        val nextOccMap = occMap
          .toSeq
          .flatMap { case (pair, occ) =>
            (pair.head + rules(pair) + pair.last)
              .sliding(2).map(_ -> occ).toSeq
          }
          .groupBy(_._1)
          .map { case (pair, occs) => pair -> occs.map(_._2).sum }

        buildPolymerByMap(nextOccMap, step + 1)
      }
    }

    val occMap = template
      .sliding(2)
      .map(_ -> 1L)
      .toMap

    printer.printLine(s"Template")
    occMap.foreach { case (pair, occ) => printer.printLine(s"$pair -> $occ") }
    printer.printSoftLine()

    buildPolymerByMap(occMap, 1)
  }

  def parseInput(input: Seq[String]): (String, Map[String, String]) = {
    val template = input.head

    val rules = input.slice(2, input.length).map {
      case s"$key -> $value" => key -> value
    }.toMap

    (template, rules)
  }

  "buildPolymer" should {
    def simulateSteps(steps: Int): String = {
      printer = new Printer(false)
      stepsToSimulate = steps
      val input             = getExample()
      val (template, rules) = parseInput(input)
      val polymer           = buildPolymerBySimulation(template, rules)

      polymer
    }

    "do 1 step correctly" in {
      simulateSteps(1) shouldBe "NCNBCHB"
    }

    "do 2 step correctly" in {
      simulateSteps(2) shouldBe "NBCCNBBBCBHCB"
    }

    "do 3 step correctly" in {
      simulateSteps(3) shouldBe "NBBBCNCCNBBNBNBBCHBHHBCHB"
    }

    "do 4 step correctly" in {
      simulateSteps(4) shouldBe "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    }
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)
      stepsToSimulate = 10

      val input = getExample()
      part1(input) shouldBe 1588L
    }

    "solve the task" in {
      printer = new Printer(true)
      stepsToSimulate = 10

      val input = getTask()
      part1(input) shouldBe 2375L
    }
  }

  "Part 2" should {
    "solve the example with 10 steps" in {
      printer = new Printer(true)
      stepsToSimulate = 10

      val input = getExample()
      part2(input) shouldBe 1588L
    }

    "solve the example with 40 steps" in {
      printer = new Printer(true)
      stepsToSimulate = 40

      val input = getExample()
      part2(input) shouldBe 2188189693529L
    }

    "solve the task" in {
      printer = new Printer(true)
      stepsToSimulate = 40

      val input = getTask()
      part2(input) shouldBe 1976896901756L
    }
  }
}
