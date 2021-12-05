package year2021.day5

import util.Logger

object Day5Util {
  def getDangerousFields(input: Seq[String])(implicit log: Logger): Seq[(Int, Int)] = {
    log.info("====================")
    val ret = input.flatMap(line => {
      line match {
        case s"$x1s,$y1s -> $x2s,$y2s" =>
          val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
          val dangerousFields = {
            if (x1 == x2) {
              for (y <- orderedRangeOf(y1, y2)) yield {
                (x1, y)
              }
            } else if (y1 == y2) {
              for (x <- orderedRangeOf(x1, x2)) yield {
                (x, y1)
              }
            } else {
              Seq.empty[(Int, Int)]
            }
          }
          if (dangerousFields.nonEmpty) {
            log.info(s"Parsing $line")
            log.info(s"Dangerous fields: $dangerousFields")
            log.info("--------------------")
          }
          dangerousFields
      }
    })
    log.info("====================")
    ret
  }

  def orderedRangeOf(y1: Int, y2: Int): Seq[Int] = {
    if (y1 > y2) y2 to y1 else y1 to y2
  }

  def accumulateDangerousFields(dangerousFields: Seq[(Int, Int)])(implicit log: Logger): Map[(Int, Int), Int] = {
    log.info("====================")
    val accumulated = dangerousFields
      .groupBy { case (x, y) => (x, y) }
      .view
      .mapValues(_.length)
      .toMap
    log.info(s"Accumulated dangers are: $accumulated")
    log.info("====================")
    accumulated
  }
}
