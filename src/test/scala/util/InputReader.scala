package util

import scala.io.Source

object InputReader {
  def getExample(year: Int, day: Int): Seq[String] = {
    get(year, day, "example")
  }

  def getTask(year: Int, day: Int): Seq[String] = {
    get(year, day, "task")
  }

  def get(year: Int, day: Int, postfix: String): Seq[String] = {
    Source.fromResource(s"y$year/d${day}_$postfix.txt").getLines.toSeq
  }
}
