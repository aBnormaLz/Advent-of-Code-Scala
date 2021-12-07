package year2021.day6

import util.Printer

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object MapSolution {
  def calculateGenerationSize(initialGen: Seq[Int], daysToSimulate: Int)(implicit printerOpt: Option[Printer] = None): Long = {
    val actualGen = (0 to 8)
      .map(x => x -> initialGen.count(y => y == x).toLong)
      .toMap

    printerOpt.foreach(printer => {
      val sortedActualGen = ListMap(actualGen.toSeq.sortBy(_._1): _*)

      printer.printLine(s"initialGen: \n${sortedActualGen.mkString("\n")}")
      printer.printSoftLine()
    })

    simulate(actualGen, daysToSimulate)
  }

  @tailrec
  def simulate(actualGen: Map[Int, Long], daysToSimulate: Int)(implicit printerOpt: Option[Printer]): Long = {
    daysToSimulate match {
      case 0 => actualGen.values.sum
      case _ =>
        val nextGen = calculateNextGen(actualGen)
        printerOpt.foreach(printer => {
          val sortedNextGen = ListMap(nextGen.toSeq.sortBy(_._1): _*)

          printer.printLine(s"Days remaining: $daysToSimulate")
          printer.printLine()
          printer.printLine(s"nextGen: \n${sortedNextGen.mkString("\n")}")
          printer.printSoftLine()
        })
        simulate(nextGen, daysToSimulate - 1)
    }
  }

  private def calculateNextGen(actualGen: Map[Int, Long]): Map[Int, Long] = {
    val daysStepped = actualGen.map(x => x._1 - 1 -> x._2)

    val population8 = daysStepped.getOrElse(8, 0L) + daysStepped.getOrElse(-1, 0L)
    val population6 = daysStepped.getOrElse(6, 0L) + daysStepped.getOrElse(-1, 0L)

    daysStepped + (8 -> population8) + (6 -> population6) - -1
  }
}
