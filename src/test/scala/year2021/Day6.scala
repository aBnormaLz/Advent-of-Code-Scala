package year2021

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._
import year2021.day6._

class Day6 extends Task(2021, 6) with AnyWordSpecLike with Matchers {
  var printer: Printer = _

  case class Res(
      tail: Long,
      rec: Long,
      map: Long,
      mapElapsed: Long,
      seq: Long,
      seqElapsed: Long,
  )

  def part1(input: Seq[String]): Res = {
    val initialGen: Array[Int] = input.head.split(",").map(_.toInt)
    val daysToSimulate         = 80

    printer.printLine(s"Initial state: ${initialGen.mkString(",")}")
    printer.printTitle("TailRecursiveSolutionWithSeq")
    val tail = TailRecursiveSolutionWithSeq.calculateGenerationSize(initialGen, 0, daysToSimulate)(printer.some)
    val rec  = RecursiveSolution.calculateGenerationSize(initialGen, daysToSimulate)
    printer.printTitle("MapSolution")
    val map  = MapSolution.calculateGenerationSize(initialGen, daysToSimulate)(printer.some)
    printer.printTitle("SeqSolution")
    val seq  = SeqSolution.calculateGenerationSize(initialGen, daysToSimulate)(printer.some)

    Res(tail, rec, map, -1, seq, -1)
  }

  def part2(input: Seq[String]): Res = {
    val initialGen = input.head.split(",").map(_.toInt)

    (for (daysToSimulate <- 256 to 256) yield {
      printer.printLine(s"Calculating with $daysToSimulate days")

      val tail = -1
//      val (tail, tailElapsed) = ElapsedTimeUtil.measureElapsedTimeInMillis(
//        TailRecursiveSolutionWithSeq.calculateGenerationSize(initialGen, 0, daysToSimulate),
//      )
//      printElapsedTimeInMillis(tailElapsed, "tail-recursive-with-seq")

      val rec = -1
//      val (rec, recElapsed) = ElapsedTimeUtil.measureElapsedTimeInMillis(
//        RecursiveSolution.calculateGenerationSize(initialGen, daysToSimulate),
//      )
//      printElapsedTimeInMillis(recElapsed, "recursive")

      val (map, mapElapsed) = ElapsedTimeUtil.measureElapsedTimeInNanos(
        MapSolution.calculateGenerationSize(initialGen, daysToSimulate),
      )
      printElapsedTimeInNanos(mapElapsed, "map")

      val (seq, seqElapsed) = ElapsedTimeUtil.measureElapsedTimeInNanos(
        SeqSolution.calculateGenerationSize(initialGen, daysToSimulate),
      )
      printElapsedTimeInNanos(seqElapsed, "seq")

      val res = Res(tail, rec, map, mapElapsed, seq, seqElapsed)
      if (!(map == seq)) {
        printer.printLine(s"The results didn't match $res ")
        printer.print(s"rec: ${res.rec}, ")
        printer.print(s"map: ${res.map}, ")
        printer.printLine(s"seq: ${res.seq}")
      } else {
        printer.printLine(s"Result is: $seq")
      }
      printer.printHardLine()
      res
    }).last
  }

  def printElapsedTimeInMillis(elapsed: Long, qualifier: String): Unit = {
    printer.printLine(s"${TimeUtil.formatMillis(elapsed)} - $qualifier ($elapsed)")
  }

  def printElapsedTimeInNanos(elapsed: Long, qualifier: String): Unit = {
    printer.printLine(s"${TimeUtil.formatNanos(elapsed)} - $qualifier ($elapsed)")
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      val res   = part1(input)

      res.tail shouldBe 5934
      res.rec shouldBe 5934
      res.map shouldBe 5934
      res.seq shouldBe 5934
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      val res   = part1(input)

      res.tail shouldBe 362639
      res.rec shouldBe 362639
      res.map shouldBe 362639
      res.seq shouldBe 362639
    }
  }

  "Part 2" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      val res   = part2(input)

//      res.tail shouldBe 26984457539L
//      res.rec shouldBe 26984457539L
      res.map shouldBe 26984457539L
      res.seq shouldBe 26984457539L
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      val res   = part2(input)

//      res.tail shouldBe 1639854996917L
//      res.rec shouldBe 1639854996917L
      res.map shouldBe 1639854996917L
      res.seq shouldBe 1639854996917L
    }
  }
}
