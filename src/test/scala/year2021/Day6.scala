package year2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util._
import year2021.day6._

class Day6 extends Task(2021, 6) with AnyWordSpecLike with Matchers {
  var printer: Printer = _

  case class Res(
      tail: Long,
      rec: Long,
      fast: Long,
      fastElapsed: Long,
      seq: Long,
      seqElapsed: Long,
  )

  def part1(input: Seq[String]): Res = {
    val initialGen: Array[Int] = input.head.split(",").map(_.toInt)
    val daysToSimulate         = 80

    printer.printLine(s"Initial state: ${initialGen.mkString(",")}")
    printer.printLine()
    printer.printLine("====================================")
    printer.printLine("=== TailRecursiveSolutionWithSeq ===")
    printer.printLine("====================================")
    printer.printLine()
    val tail = TailRecursiveSolutionWithSeq.calculateGenerationSize(initialGen, 0, daysToSimulate, Some(printer))
    val rec  = RecursiveSolution.calculateGenerationSize(initialGen, daysToSimulate)
    printer.printLine()
    printer.printLine("=======================")
    printer.printLine("=== FastMapSolution ===")
    printer.printLine("=======================")
    printer.printLine()
    val fast = FastMapSolution.calculateGenerationSize(initialGen, daysToSimulate, Some(printer))
    printer.printLine()
    printer.printLine("=========================")
    printer.printLine("=== SimpleSeqSolution ===")
    printer.printLine("=========================")
    printer.printLine()
    val seq  = SimpleSeqSolution.calculateGenerationSize(initialGen, daysToSimulate, Some(printer))

    Res(tail, rec, fast, -1, seq, -1)
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

      val rec               = -1
//      val (rec, recElapsed) = ElapsedTimeUtil.measureElapsedTimeInMillis(
//        RecursiveSolution.calculateGenerationSize(initialGen, daysToSimulate),
//      )
//      printElapsedTimeInMillis(recElapsed, "recursive")

      val (fast, fastElapsed) = ElapsedTimeUtil.measureElapsedTimeInNanos(
        FastMapSolution.calculateGenerationSize(initialGen, daysToSimulate),
      )
      printElapsedTimeInNanos(fastElapsed, "fast-map")

      val (seq, seqElapsed) = ElapsedTimeUtil.measureElapsedTimeInNanos(
        SimpleSeqSolution.calculateGenerationSize(initialGen, daysToSimulate),
      )
      printElapsedTimeInNanos(seqElapsed, "seq")

      val res = Res(tail, rec, fast, fastElapsed, seq, seqElapsed)
      if (!(fast == seq)) {
        printer.printLine(s"The results didn't match $res ")
        printer.print(s"rec: ${res.rec}, ")
        printer.print(s"fast: ${res.fast}, ")
        printer.printLine(s"seq: ${res.seq}")
      } else {
        printer.printLine(s"Result is: $seq")
      }
      printer.printLine("====================")
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
      res.fast shouldBe 5934
      res.seq shouldBe 5934
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      val res   = part1(input)

      res.tail shouldBe 362639
      res.rec shouldBe 362639
      res.fast shouldBe 362639
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
      res.fast shouldBe 26984457539L
      res.seq shouldBe 26984457539L
    }

    "solve the task" in {
      printer = new Printer(true)

      val input = getTask()
      val res   = part2(input)

//      res.tail shouldBe 1639854996917L
//      res.rec shouldBe 1639854996917L
      res.fast shouldBe 1639854996917L
      res.seq shouldBe 1639854996917L
    }
  }
}
