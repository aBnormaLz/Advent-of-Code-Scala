package util

import scala.io.Source

abstract class Task(val year: Int, val day: Int) {
  implicit var printer: Printer = _

  def getExample(specifier: Int): Seq[String] = {
    get("example" + specifier)
  }

  def getExample(): Seq[String] = {
    get("example")
  }

  def getTask(): Seq[String] = {
    get("task")
  }

  private def get(specifier: String): Seq[String] = {
    Source.fromResource(s"year$year/day${day}_$specifier.txt").getLines.toSeq
  }
}
