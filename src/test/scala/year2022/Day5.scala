package year2022

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util.Ops.{IntOps, SeqOps, StringOps}
import util.{Printer, Task}

class Day5 extends Task(2022, 5) with AnyWordSpecLike with Matchers {
  type Cargo = Seq[Seq[String]]

  def parseCargo(input: Seq[String]): Cargo = {
    input.split("")
      .checkSizeEquals(2)
      .head.init
      .map { line =>
        (line + " ").sliding(4, 4).map {
          {
            case s"[$char] " => char
            case _           => "N/A"
          }
        }.toSeq
      }.reverse
      .transpose
      .map(_.filter(_ != "N/A"))
  }

  implicit class CargoOps(cargo: Cargo) {
    private val cargoHeight = cargo.map(_.size).max - 1
    private val stackWidth  = cargo.size

    def run(command: Command, reverseMovedCrates: Boolean): Cargo = {
      printer.printLine(s"Executing command: ${command.toPrettyString()}")
      val (newFrom, cratesToMove) = cargo(command.from - 1).splitAt(cargo(command.from - 1).size - command.crates)
      val actuallyMovedCrates     = if (reverseMovedCrates) cratesToMove.reverse else cratesToMove
      val newTo                   = cargo(command.to - 1) ++ actuallyMovedCrates

      cargo
        .updated(command.from - 1, newFrom)
        .updated(command.to - 1, newTo)
    }

    def getTopLayer(): Iterable[String] = {
      cargo.map(_.last)
    }

    def toPrettyString(): String = {
      val cargoString = (0 to cargoHeight).reverse.map { level =>
        (0 until stackWidth).map { stack =>
          if (cargo(stack).size > level) {
            s"[${cargo(stack)(level)}] "
          } else {
            "    "
          }
        }.mkString("").trimRight
      }.mkString("\n")

      val stackString = (1 to stackWidth).mkString("   ").trimRight

      (cargoString, "\n ", stackString).productIterator.mkString("")
    }
  }

  def parseCommands(input: Seq[String], stepsToSimulate: Int = 0): Seq[Command] = {
    val commandStrings = input.split("").checkSizeEquals(2)(1)
    Command(
      if (stepsToSimulate == 0) {
        commandStrings
      } else {
        val allCommands = commandStrings
        commandStrings.slice(0, stepsToSimulate.butMax(allCommands.size))
      },
    )
  }

  case class Command(crates: Int, from: Int, to: Int) {
    def toPrettyString(): String = {
      s"move $crates from $from to $to"
    }
  }

  object Command {
    def apply(commands: Seq[String]): Seq[Command] = {
      commands.map {
        case s"move $crates from $from to $to" => Command(crates.toInt, from.toInt, to.toInt)
        case other                             => throw new UnknownError(s"Unexpected command: $other")
      }
    }
  }

  def simulate(cargo: Cargo, commands: Seq[Command], reverseMovedCrates: Boolean): Cargo = {
    commands.foldLeft(cargo) { (prev, command) =>
      val newCargo = prev.run(command, reverseMovedCrates)
      printer.printLine(newCargo.toPrettyString())
      printer.printSoftLine()
      newCargo
    }
  }

  def solution(input: Seq[String], reverseMovedCrates: Boolean): String = {
    val cargo = parseCargo(input)
    printer.printLine(cargo.toPrettyString())
    printer.printSoftLine()

    val commands   = parseCommands(input)
    val finalCargo = simulate(cargo, commands, reverseMovedCrates)

    finalCargo.getTopLayer().mkString("")
  }

  "Part 1" should {
    "parse and print cargo correctly" in {
      parseCargo(getExample()).toPrettyString() shouldBe
        """
          |    [D]
          |[N] [C]
          |[Z] [M] [P]
          | 1   2   3""".stripMargin.tail
    }

    "simulate 1 step correctly" in {
      printer = new Printer(false)
      val input        = getExample()
      val initialCargo = parseCargo(input)
      val commands     = parseCommands(input, 1)
      val cargo        = simulate(initialCargo, commands, reverseMovedCrates = true)
      cargo.toPrettyString() shouldBe
        """
          |[D]
          |[N] [C]
          |[Z] [M] [P]
          | 1   2   3""".stripMargin.tail
    }

    "simulate 2 step correctly" in {
      printer = new Printer(false)
      val input        = getExample()
      val initialCargo = parseCargo(input)
      val commands     = parseCommands(input, 2)
      val cargo        = simulate(initialCargo, commands, reverseMovedCrates = true)
      cargo.toPrettyString() shouldBe
        """
          |        [Z]
          |        [N]
          |    [C] [D]
          |    [M] [P]
          | 1   2   3""".stripMargin.tail
    }

    "simulate 3 step correctly" in {
      printer = new Printer(false)
      val input        = getExample()
      val initialCargo = parseCargo(input)
      val commands     = parseCommands(input, 3)
      val cargo        = simulate(initialCargo, commands, reverseMovedCrates = true)
      cargo.toPrettyString() shouldBe
        """
          |        [Z]
          |        [N]
          |[M]     [D]
          |[C]     [P]
          | 1   2   3""".stripMargin.tail
    }

    "simulate 4 step correctly" in {
      printer = new Printer(false)
      val input        = getExample()
      val initialCargo = parseCargo(input)
      val commands     = parseCommands(input, 4)
      val cargo        = simulate(initialCargo, commands, reverseMovedCrates = true)
      cargo.toPrettyString() shouldBe
        """
          |        [Z]
          |        [N]
          |        [D]
          |[C] [M] [P]
          | 1   2   3""".stripMargin.tail
    }

    "solve the example" in {
      printer = new Printer(true)
      val input = getExample()
      solution(input, reverseMovedCrates = true) shouldBe "CMZ"
    }

    "solve the task" in {
      printer = new Printer(true)
      val input = getTask()
      solution(input, reverseMovedCrates = true) shouldBe "MQSHJMWNH"
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)
      val input = getExample()
      solution(input, reverseMovedCrates = false) shouldBe "MCD"
    }

    "solve the task" in {
      printer = new Printer(true)
      val input = getTask()
      solution(input, reverseMovedCrates = false) shouldBe "LLWJRBHVZ"
    }
  }
}
