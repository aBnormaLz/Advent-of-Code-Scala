package year2021.day6

import util.Printer
import util.RichString.PadLeftTo

import scala.annotation.tailrec

object TailRecursiveSolutionWithSeq {
  def calculateGenerationSize(initialGen: Seq[Int], genNumber: Int, daysToSimulate: Int)(implicit printerOpt: Option[Printer] = None): Long = {
    calculateNextGen(initialGen, genNumber, daysToSimulate).length
  }

  @tailrec
  private def calculateNextGen(spawns: Seq[Int], genNumber: Int, daysToSimulate: Int)(implicit printerOpt: Option[Printer]): Seq[Int] = {
    if (genNumber >= daysToSimulate) {
      spawns
    } else {
      val numberOfSpawns    = spawns.count(_ == 0)
      val decrementedSpawns = spawns
        .map(spawn =>
          if (spawn == 0) {
            6
          } else {
            spawn - 1
          },
        )

      val spawned              = Seq.fill(numberOfSpawns)(8)
      val nextSpawns: Seq[Int] = decrementedSpawns ++ spawned

      printerOpt.foreach(printer => {
        val paddedElapsedDays =
          if (genNumber == 1) {
            s"${(genNumber + 1).toString.padLeftTo(2, " ").mkString("")} day "
          } else {
            s"${(genNumber + 1).toString.padLeftTo(2, " ").mkString("")} days"
          }
        printer.printLine(s"After $paddedElapsedDays: ${nextSpawns.mkString(",")}")
      })
      calculateNextGen(nextSpawns, genNumber + 1, daysToSimulate)
    }
  }
}
