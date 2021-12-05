package util

class Printer(printingEnabled: Boolean) {
  def ifEnabled()(callback: => Unit): Unit = {
    if (printingEnabled) {
      callback
    }
  }

  def printLine(): Unit = {
    ifEnabled() {
      println()
    }
  }

  def printLine(str: String): Unit = {
    ifEnabled() {
      println(str)
    }
  }

  def print(str: String): Unit = {
    ifEnabled() {
      print(str)
    }
  }
}
