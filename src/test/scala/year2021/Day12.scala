package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util._
import year2021.day12.Util.{CaveSystem, Connection}

class Day12 extends Task(2021, 12) with AnyWordSpecLike with Matchers {
  def part1(input: Seq[String]): Set[String] = {
    val caveSystem = CaveSystem(input.map(Connection.apply))

    val paths = tracePaths1(
      caveSystem = caveSystem,
      actualNode = "start",
      actualPath = Seq(),
      paths = Set(),
    )

    paths.map(_.mkString(","))
  }

  def tracePaths1(
      caveSystem: CaveSystem,
      actualNode: String,
      actualPath: Seq[String],
      paths: Set[Seq[String]],
  ): Set[Seq[String]] = {
    val nextActualPath           = actualPath :+ actualNode
    val nextActualLowercasePath  = actualPath.filter(name => name.toLowerCase == name)
    val actualNeighboursFiltered = caveSystem.getNeighboursOf(actualNode).filterNot(nextActualLowercasePath.contains)
    (for (nextNode <- actualNeighboursFiltered) yield {
      printer.printInfo(
        "actualPath: "               -> actualPath.toString(),
        "actualNode: "               -> actualNode,
        "nextActualLowercasePath: "  -> nextActualLowercasePath.toString(),
        "actualNeighboursFiltered: " -> actualNeighboursFiltered.toString(),
        "nextActualPath: "           -> nextActualPath.toString(),
        "nextNode: "                 -> nextNode,
      )

      nextNode match {
        case "end" =>
          val pathToAdd = nextActualPath :+ nextNode
          printer.printLine(s"Next node is end, adding: $pathToAdd")
          printer.printSoftLine()
          paths + pathToAdd
        case _     =>
          printer.printSoftLine()
          tracePaths1(caveSystem, nextNode, nextActualPath, paths)
      }
    }).flatten.toSet
  }

  def part2(input: Seq[String]): Set[String] = {
    val caveSystem = CaveSystem(input.map(Connection.apply))

    val paths = tracePaths2(
      caveSystem = caveSystem,
      actualNode = "start",
      actualPath = Seq(),
      paths = Set(),
    )

    paths.map(_.mkString(","))
  }

  def tracePaths2(
      caveSystem: CaveSystem,
      actualNode: String,
      actualPath: Seq[String],
      paths: Set[Seq[String]],
  ): Set[Seq[String]] = {
    val nextActualPath           = actualPath :+ actualNode
    val duplicatedLowercasePaths = nextActualPath
      .filter(_ != "start")
      .filter(_ != "end")
      .filter(name => name.toLowerCase == name)
      .groupBy(identity)
      .collect {
        case (key, value) if value.length > 1 => key
      }
      .checkSizeMax(1)

    val nextActualLowercasePath  = actualPath.filter(name => name.toLowerCase == name)
    val neighbours               = caveSystem.getNeighboursOf(actualNode)
    val actualNeighboursFiltered =
      if (duplicatedLowercasePaths.isEmpty) {
        neighbours
          .filter(_ != "start")
      } else {
        neighbours
          .filterNot(nextActualLowercasePath.contains)
      }
    (for (nextNode <- actualNeighboursFiltered) yield {
      printer.printInfo(
        "actualPath: "               -> actualPath.toString(),
        "actualNode: "               -> actualNode,
        "nextActualLowercasePath: "  -> nextActualLowercasePath.toString(),
        "actualNeighboursFiltered: " -> actualNeighboursFiltered.toString(),
        "nextActualPath: "           -> nextActualPath.toString(),
        "nextNode: "                 -> nextNode,
      )

      nextNode match {
        case "end" =>
          val pathToAdd = nextActualPath :+ nextNode
          printer.printLine(s"Next node is end, adding: $pathToAdd")
          printer.printSoftLine()
          paths + pathToAdd
        case _     =>
          printer.printSoftLine()
          tracePaths2(caveSystem, nextNode, nextActualPath, paths)
      }
    }).flatten.toSet
  }

  "Part 1" should {
    "solve the example 1" in {
      printer = new Printer(false)

      val input    = getExample(1)
      val solution = part1(input)
      solution.contains("start,A,b,A,c,A,end") shouldBe true
      solution.contains("start,A,b,A,end") shouldBe true
      solution.contains("start,A,b,end") shouldBe true
      solution.contains("start,A,c,A,b,A,end") shouldBe true
      solution.contains("start,A,c,A,b,end") shouldBe true
      solution.contains("start,A,c,A,end") shouldBe true
      solution.contains("start,A,end") shouldBe true
      solution.contains("start,b,A,c,A,end") shouldBe true
      solution.contains("start,b,A,end") shouldBe true
      solution.contains("start,b,end") shouldBe true
    }

    "solve the example 2" in {
      printer = new Printer(false)

      val input    = getExample(2)
      val solution = part2(input)
      solution.contains("start,HN,dc,HN,end") shouldBe true
      solution.contains("start,HN,dc,HN,kj,HN,end") shouldBe true
      solution.contains("start,HN,dc,end") shouldBe true
      solution.contains("start,HN,dc,kj,HN,end") shouldBe true
      solution.contains("start,HN,end") shouldBe true
      solution.contains("start,HN,kj,HN,dc,HN,end") shouldBe true
      solution.contains("start,HN,kj,HN,dc,end") shouldBe true
      solution.contains("start,HN,kj,HN,end") shouldBe true
      solution.contains("start,HN,kj,dc,HN,end") shouldBe true
      solution.contains("start,HN,kj,dc,end") shouldBe true
      solution.contains("start,dc,HN,end") shouldBe true
      solution.contains("start,dc,HN,kj,HN,end") shouldBe true
      solution.contains("start,dc,end") shouldBe true
      solution.contains("start,dc,kj,HN,end") shouldBe true
      solution.contains("start,kj,HN,dc,HN,end") shouldBe true
      solution.contains("start,kj,HN,dc,end") shouldBe true
      solution.contains("start,kj,HN,end") shouldBe true
      solution.contains("start,kj,dc,HN,end") shouldBe true
      solution.contains("start,kj,dc,end") shouldBe true
    }

    "solve the example 3" in {
      printer = new Printer(false)

      val input = getExample(3)
      part1(input).size shouldBe 226
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      part1(input).size shouldBe 3576
    }
  }

  "Part 2" should {
    "solve the example 1" in {
      printer = new Printer(false)

      val input    = getExample(1)
      val solution = part2(input)
      solution.contains("start,A,b,A,b,A,c,A,end") shouldBe true
      solution.contains("start,A,b,A,b,A,end") shouldBe true
      solution.contains("start,A,b,A,b,end") shouldBe true
      solution.contains("start,A,b,A,c,A,b,A,end") shouldBe true
      solution.contains("start,A,b,A,c,A,b,end") shouldBe true
      solution.contains("start,A,b,A,c,A,c,A,end") shouldBe true
      solution.contains("start,A,b,A,c,A,end") shouldBe true
      solution.contains("start,A,b,A,end") shouldBe true
      solution.contains("start,A,b,d,b,A,c,A,end") shouldBe true
      solution.contains("start,A,b,d,b,A,end") shouldBe true
      solution.contains("start,A,b,d,b,end") shouldBe true
      solution.contains("start,A,b,end") shouldBe true
      solution.contains("start,A,c,A,b,A,b,A,end") shouldBe true
      solution.contains("start,A,c,A,b,A,b,end") shouldBe true
      solution.contains("start,A,c,A,b,A,c,A,end") shouldBe true
      solution.contains("start,A,c,A,b,A,end") shouldBe true
      solution.contains("start,A,c,A,b,d,b,A,end") shouldBe true
      solution.contains("start,A,c,A,b,d,b,end") shouldBe true
      solution.contains("start,A,c,A,b,end") shouldBe true
      solution.contains("start,A,c,A,c,A,b,A,end") shouldBe true
      solution.contains("start,A,c,A,c,A,b,end") shouldBe true
      solution.contains("start,A,c,A,c,A,end") shouldBe true
      solution.contains("start,A,c,A,end") shouldBe true
      solution.contains("start,A,end") shouldBe true
      solution.contains("start,b,A,b,A,c,A,end") shouldBe true
      solution.contains("start,b,A,b,A,end") shouldBe true
      solution.contains("start,b,A,b,end") shouldBe true
      solution.contains("start,b,A,c,A,b,A,end") shouldBe true
      solution.contains("start,b,A,c,A,b,end") shouldBe true
      solution.contains("start,b,A,c,A,c,A,end") shouldBe true
      solution.contains("start,b,A,c,A,end") shouldBe true
      solution.contains("start,b,A,end") shouldBe true
      solution.contains("start,b,d,b,A,c,A,end") shouldBe true
      solution.contains("start,b,d,b,A,end") shouldBe true
      solution.contains("start,b,d,b,end") shouldBe true
      solution.contains("start,b,end") shouldBe true
    }

    "solve the example 2" in {
      printer = new Printer(false)

      val input = getExample(2)
      part2(input).size shouldBe 103
    }

    "solve the example 3" in {
      printer = new Printer(false)

      val input = getExample(3)
      part2(input).size shouldBe 3509
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      part2(input).size shouldBe 84271
    }
  }
}
