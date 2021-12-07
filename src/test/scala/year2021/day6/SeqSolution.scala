package year2021.day6

import util.Printer

import scala.annotation.tailrec

object SeqSolution {
  def calculateGenerationSize(initialGen: Seq[Int], daysToSimulate: Int)(implicit printerOpt: Option[Printer] = None): Long = {
    val actualGen = (0 to 8)
      .map(x => initialGen.count(y => y == x).toLong)

    printerOpt.foreach(printer => {
      printer.printLine(s"initialGen: \n${actualGen.zipWithIndex.map(_.swap).mkString("\n")}\n")
      printer.printLine("--------------------")
    })

    simulate(actualGen, daysToSimulate)
  }

  @tailrec
  def simulate(actualGen: Seq[Long], daysToSimulate: Int)(implicit printerOpt: Option[Printer]): Long = {
    daysToSimulate match {
      case 0 => actualGen.sum
      case _ =>
        val nextGen = calculateNextGen(actualGen)
        printerOpt.foreach(printer => {
          printer.printLine(s"Days remaining: ${daysToSimulate - 1}")
          printer.printLine()
          printer.printLine(s"nextGen: \n${actualGen.zipWithIndex.map(_.swap).mkString("\n")}")
          printer.printLine("--------------------")
        })
        simulate(nextGen, daysToSimulate - 1)
    }
  }

  private def calculateNextGen(actualGen: Seq[Long]): Seq[Long] = {
    Seq(
      actualGen(1),
      actualGen(2),
      actualGen(3),
      actualGen(4),
      actualGen(5),
      actualGen(6),
      actualGen(7) + actualGen.head,
      actualGen(8),
      actualGen.head,
    )
  }
}
