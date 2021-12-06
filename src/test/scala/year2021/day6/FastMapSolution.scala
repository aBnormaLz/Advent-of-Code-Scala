package year2021.day6

import util.Printer

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object FastMapSolution {
  def calculateGenerationSize(initialGen: Seq[Int], daysToSimulate: Int, printerOpt: Option[Printer] = None): Long = {
    val actualGen = (0 to 8)
      .map(x => x -> initialGen.count(y => y == x).toLong)
      .toMap

    printerOpt.foreach(printer => {
      val sortedActualGen = ListMap(actualGen.toSeq.sortBy(_._1): _*)

      printer.printLine(s"initialGen: \n${sortedActualGen.mkString("\n")}")
      printer.printLine("--------------------")
    })

    simulateDay(actualGen, daysToSimulate, printerOpt)
  }

  private def calculateNextGen(actualGen: Map[Int, Long]): Map[Int, Long] = {
    val daysStepped: Map[Int, Long] = actualGen.map(x => x._1 - 1 -> x._2)

    val population8: Long = daysStepped.getOrElse(8, 0L) + daysStepped.getOrElse(-1, 0L)
    val population6: Long = daysStepped.getOrElse(6, 0L) + daysStepped.getOrElse(-1, 0L)

    daysStepped + (8 -> population8) + (6 -> population6) - -1
  }

  @tailrec
  def simulateDay(actualGen: Map[Int, Long], daysToSimulate: Int, printerOpt: Option[Printer]): Long = {
    daysToSimulate match {
      case 0 => actualGen.values.sum
      case _ =>
        val nextGen = calculateNextGen(actualGen)
        printerOpt.foreach(printer => {
          val sortedNextGen   = ListMap(nextGen.toSeq.sortBy(_._1): _*)

          printer.printLine(s"Days remaining: $daysToSimulate")
          printer.printLine()
          printer.printLine(s"nextGen: \n${sortedNextGen.mkString("\n")}")
          printer.printLine("--------------------")
        })
        simulateDay(nextGen, daysToSimulate - 1, printerOpt: Option[Printer])
    }
  }
}
