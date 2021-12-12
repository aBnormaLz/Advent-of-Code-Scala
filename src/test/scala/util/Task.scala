package util

import scala.io.Source

abstract class Task(val year: Int, val day: Int) {
  def getExample(specifier: Int): Seq[String] = {
    get(year, day, "example" + specifier)
  }

  def getExample(): Seq[String] = {
    get(year, day, "example")
  }

  def getTask(): Seq[String] = {
    get(year, day, "task")
  }

  private def get(year: Int, day: Int, specifier: String): Seq[String] = {
    Source.fromResource(s"year$year/day${day}_$specifier.txt").getLines.toSeq
  }
}
