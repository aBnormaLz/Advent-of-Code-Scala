package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Printer, Task}

class Day4 extends Task(2021, 4) with AnyWordSpecLike with Matchers {
  val printer: Printer = new Printer(true)

  def part1(input: Seq[String]): Int = {
    val allWinningNumbers              = input.head.split(",").map(_.toInt)
    val rawTables                      = parseBingos(input)
    val (winningTable, lastDrewNumber) = findWinningTableAndLastNumber(rawTables, allWinningNumbers)

    winningTable.sumOfNotWinningNumbers() * lastDrewNumber
  }

  def part2(input: Seq[String]): Int = {
    val allWinningNumbers              = input.head.split(",").map(_.toInt)
    val rawTables                      = parseBingos(input)
    val (winningTable, lastDrewNumber) = findLastWinningTableAndLastNumber(rawTables, allWinningNumbers)

    winningTable.sumOfNotWinningNumbers() * lastDrewNumber
  }

  case class BingoNumber(number: Int, isWining: Boolean)

  object BingoNumber {
    def isLineWinning(line: Seq[BingoNumber]): Boolean = {
      line.forall(_.isWining)
    }
  }
  import BingoNumber._

  case class BingoTable(numbers: Seq[Seq[Int]], drewNumbers: Seq[Int]) {
    def prettyToString(): String = {
      numbers.map { line =>
        line.map { number =>
          val padded = number.toString.reverse.padTo(2, " ").reverse.mkString("")
          if (drewNumbers.contains(number)) {
            val ret = s"($padded)"
            ret
          } else {
            val ret = s" $padded "
            ret
          }
        }.mkString(" ")
      }.mkString("\n") + "\n"
    }

    def toWinningTable(): Seq[Seq[BingoNumber]] = {
      numbers.map { line =>
        line.map { number =>
          BingoNumber(number, drewNumbers.contains(number))
        }
      }
    }

    def isWinning(): Boolean = {
      val winningTable    = toWinningTable()
      val isAnyRowWinning = winningTable.exists(isLineWinning)
      val isAnyColWinning = winningTable.transpose.exists(isLineWinning)

      isAnyRowWinning || isAnyColWinning
    }

    def sumOfNotWinningNumbers(): Int = {
      toWinningTable()
        .map(line => {
          line
            .filterNot(_.isWining)
            .map(_.number)
            .sum
        })
        .sum
    }
  }

  def parseBingos(input: Seq[String]): Seq[Seq[Seq[Int]]] = {
    val numberOfBingos = (input.length - 1) / 6
    printer.printLine(s"numberOfBingos: $numberOfBingos")
    printer.printLine()

    for (i <- 0 until numberOfBingos) yield {
      val startOfBingo = (i * 6) + 2
      Seq(
        input(startOfBingo),
        input(startOfBingo + 1),
        input(startOfBingo + 2),
        input(startOfBingo + 3),
        input(startOfBingo + 4),
      )
        .map(_.trim.split(" +").toSeq)
        .map(_.map(rawNumber => rawNumber.toInt))
    }
  }

  def findWinningTableAndLastNumber(rawTables: Seq[Seq[Seq[Int]]], allWinningNumbers: Seq[Int]): (BingoTable, Int) = {
    var i                        = 1
    var winningTableFound        = false
    var winningTable: BingoTable = null

    while (!winningTableFound) {
      printer.printLine("====================")
      val drewNumbers = allWinningNumbers.slice(0, i)
      printer.printLine(s"Drew numbers: ${drewNumbers.mkString(", ")}")
      printer.printLine()
      rawTables.foreach(rawTable => {
        val bingoTable = BingoTable(rawTable, drewNumbers)
        printer.printLine(bingoTable.prettyToString())
      })

      val winningTables = rawTables
        .map(rawTable => BingoTable(rawTable, drewNumbers))
        .filter(_.isWinning())

      winningTables match {
        case Seq(table) =>
          winningTableFound = true
          winningTable = table
          printer.printLine("Winning table found!!")
          printer.printLine()
          printer.printLine(winningTable.prettyToString())
        case Seq(_, _*) =>
          throw new IllegalStateException("More than one winning table found!")
        case _          =>
          i = i + 1
          printer.printLine("No winning table found...")
      }
      printer.printLine("====================")
    }

    (winningTable, allWinningNumbers(i - 1))
  }

  def findLastWinningTableAndLastNumber(rawTables: Seq[Seq[Seq[Int]]], allWinningNumbers: Seq[Int]): (BingoTable, Int) = {
    var i                                 = 1
    var lastWinningTableFound             = false
    var lastWinningTable: BingoTable      = null
    var nonWinningTables: Seq[BingoTable] = rawTables
      .map(rawTable => BingoTable(rawTable, Seq()))

    while (!lastWinningTableFound) {
      printer.printLine("====================")
      val drewNumbers = allWinningNumbers.slice(0, i)
      printer.printLine(s"Drew numbers: ${drewNumbers.mkString(", ")}")
      printer.printLine()
      nonWinningTables.foreach(table => {
        val bingoTable = table.copy(drewNumbers = drewNumbers)
        printer.printLine(bingoTable.prettyToString())
      })

      val newNonWinningTables = nonWinningTables
        .map(table => table.copy(drewNumbers = drewNumbers))
        .filterNot(_.isWinning())

      if (newNonWinningTables.isEmpty) {

        nonWinningTables match {
          case Seq(table) =>
            lastWinningTableFound = true
            lastWinningTable = table.copy(drewNumbers = drewNumbers)
            printer.printLine("Last winning table found!!")
          case Seq(_, _*) =>
            throw new IllegalStateException("More than one last winning table found!")
        }
      } else {
        i = i + 1
        printer.printLine("Still checking...")
      }
      nonWinningTables = newNonWinningTables
      printer.printLine("====================")
    }

    (lastWinningTable, allWinningNumbers(i - 1))
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 4512
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 65325
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 1924
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 4624
    }
  }
}
