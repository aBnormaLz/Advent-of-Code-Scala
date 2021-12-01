package util

import scala.io.Source

abstract class Task(val year: Int, val day: Int) {
  def getExample(): Seq[String] = {
    get(year, day, "example")
  }

  def getTask(): Seq[String] = {
    get(year, day, "task")
  }

  private def get(year: Int, day: Int, prefix: String): Seq[String] = {
    Source.fromResource(s"year$year/${prefix}_day$day.txt").getLines.toSeq
  }
}
