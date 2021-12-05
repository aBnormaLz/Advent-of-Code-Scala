package util

class Printer(printingEnabled: Boolean) {
  def printLine(): Unit = {
    if (printingEnabled) {
      println()
    }
  }

  def printLine(str: String): Unit = {
    if (printingEnabled) {
      println(str)
    }
  }

  def print(str: String): Unit = {
    if (printingEnabled) {
      print(str)
    }
  }
}
