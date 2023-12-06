package year2023

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.Ops.{IntCoord, IntCoordOps, IntCoordsOps}
import util.{Printer, Task}

class Day3 extends Task(2023, 3) with AnyWordSpecLike with Matchers {

  case class NumberInfo(
      number: Int,
      coordinates: Seq[IntCoord],
      adjacentSymbols: Set[SymbolInfo],
  )

  case class SymbolInfo(
      coordinate: IntCoord,
      symbol: String,
  )

  def parseNumberInfos(map: Seq[String]): Seq[NumberInfo] = {
    val min = (0, 0)
    val max = (map.head.length - 1, map.length - 1)

    map.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        val isPreviousDigit =
          if (x == 0) {
            false
          } else {
            Character.isDigit(line(x - 1))
          }

        if (Character.isDigit(char) && !isPreviousDigit) {
          val remainingOfLine = line.slice(x, line.length)
          val number          = remainingOfLine.takeWhile(Character.isDigit)

          val coordsOfNumber = for (beginning <- x until x + number.length) yield {
            (beginning, y)
          }

          val adjacentSymbols = coordsOfNumber
            .neighbours()
            .filter(_.isIn(min, max))
            .flatMap { case (x, y) =>
              val maybeSymbol = map(y)(x).toString
              if (!"[0-9]|\\.".r.matches(maybeSymbol)) {
                Some((x, y) -> maybeSymbol)
              } else {
                None
              }
            }.map { case (coord, symbol) =>
              SymbolInfo(coord, symbol)
            }
          Some(NumberInfo(number.toInt, coordsOfNumber, adjacentSymbols))
        } else {
          None
        }
      }
    }.flatten
  }

  def part1(input: Seq[String]): Int = {
    val numberInfos = parseNumberInfos(input)

    for (y <- input.indices) {
      for (x <- input.head.indices) {
        val numberToPrint = numberInfos
          .filter(_.coordinates.contains((x, y)))
          .checkSizeMax(1)
          .headOption

        val symbolToPrint = numberInfos
          .flatMap(numberInfo => {
            numberInfo.adjacentSymbols
          })
          .toSet
          .filter(_.coordinate == (x, y))
          .checkSizeMax(1)
          .headOption

        val toPrint: String = numberToPrint
          .map(numberInfo => {
            numberInfo.number.toString.charAt(x - numberInfo.coordinates.head._1).toString
          })
          .getOrElse(symbolToPrint.map(_.symbol).getOrElse("."))

        printer.print(toPrint)

      }
      printer.printLine()
    }
    printer.printLine()
    printer.printLine()

    numberInfos
      .filter(_.adjacentSymbols.nonEmpty)
      .map(_.number)
      .sum
  }

//  def part2(input: Seq[String]): Int = {
//  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(false)

      val input = getExample()
      part1(input) shouldBe 4361
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      part1(input) shouldBe 554003
    }
  }

//  "Part 2" should {
//    "solve the example" in {
//      val input = getExample()
//      part2(input) shouldBe 2286
//    }
//
//    "solve the task" in {
//      val input = getTask()
//      part2(input) shouldBe 66363
//    }
//  }
}
