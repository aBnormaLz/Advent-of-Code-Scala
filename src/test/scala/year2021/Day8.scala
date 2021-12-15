package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Assertion.CheckSize
import util._

class Day8 extends Task(2021, 8) with AnyWordSpecLike with Matchers {
  var printer: Printer = _

  def part1(input: Seq[String]): Int = {
    val samplesAndDigitsSeq = parseInput(input)
      .map {
        case (_, digits) =>
          digits.count(digit => Seq(2, 3, 4, 7).contains(digit.length))
      }
    samplesAndDigitsSeq.sum
  }

  def part2(input: Seq[String]): Int = {
    val samplesAndDigitsSeq = parseInput(input)
      .map {
        case (samples, digits) =>
          val decoder = buildDecoder(samples.map(_.toSet))
          digits
            .map(digit => decoder(digit.toSet))
            .mkString
            .toInt
      }
    samplesAndDigitsSeq.sum
  }

  def parseInput(input: Seq[String]): Seq[(Seq[String], Seq[String])] = {
    input.map(line => {
      val split   = line.split(" \\| ")
      val samples = split.head.split(" ").toSeq
      val digits  = split.last.split(" ").toSeq
      (samples, digits)
    })
  }

  /**   0: (6)  1: (2)  2: (5)  3: (5)  4: (4)
    *    aaaa            aaaa    aaaa
    *   b    c       c       c       c  b    c
    *   b    c       c       c       c  b    c
    *                    dddd    dddd    dddd
    *   e    f       f  e            f       f
    *   e    f       f  e            f       f
    *    gggg            gggg    gggg
    *
    *   5: (5)  6: (6)  7: (3)  8: (7)  9: (6)
    *    aaaa    aaaa    aaaa    aaaa    aaaa
    *   b       b            c  b    c  b    c
    *   b       b            c  b    c  b    c
    *    dddd    dddd            dddd    dddd
    *        f  e    f       f  e    f       f
    *        f  e    f       f  e    f       f
    *    gggg    gggg            gggg    gggg
    */
  def buildDecoder(samples: Seq[Set[Char]]): Map[Set[Char], String] = {
    val one   = samples.filter(_.size == 2).checkSizeEquals(1).head
    val four  = samples.filter(_.size == 4).checkSizeEquals(1).head
    val seven = samples.filter(_.size == 3).checkSizeEquals(1).head
    val eight = samples.filter(_.size == 7).checkSizeEquals(1).head

    val TWOorTHREEorFIVE = samples.filter(_.size == 5).toSet.checkSizeEquals(3)
    val ZEROorSIXorNINE  = samples.filter(_.size == 6).toSet.checkSizeEquals(3)

    val nine = ZEROorSIXorNINE
      .filter(number => (number -- four).size == 2)
      .checkSizeEquals(1)
      .head

    val six = ZEROorSIXorNINE
      .filter(number => (number -- seven).size == 4)
      .checkSizeEquals(1)
      .head

    val zero = (ZEROorSIXorNINE - six - nine)
      .checkSizeEquals(1)
      .head

    val three = TWOorTHREEorFIVE
      .filter(number => (number -- one).size == 3)
      .checkSizeEquals(1)
      .head

    val five = TWOorTHREEorFIVE
      .filter(number => (number -- six).isEmpty)
      .checkSizeEquals(1)
      .head

    val two = (TWOorTHREEorFIVE - three - five)
      .checkSizeEquals(1)
      .head

    Map(
      zero  -> "0",
      one   -> "1",
      two   -> "2",
      three -> "3",
      four  -> "4",
      five  -> "5",
      six   -> "6",
      seven -> "7",
      eight -> "8",
      nine  -> "9",
    )
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 26
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part1(input) shouldBe 367
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part2(input) shouldBe 61229
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      part2(input) shouldBe 974512
    }
  }
}
