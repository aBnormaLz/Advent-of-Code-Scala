package util

class Printer(printingEnabled: Boolean) {
  def ifEnabled()(callback: => Unit): Unit = {
    if (printingEnabled) {
      callback
    }
  }

  def printLine(): Unit = {
    ifEnabled() {
      scala.Predef.println()
    }
  }

  def printLine(str: String): Unit = {
    ifEnabled() {
      scala.Predef.println(str)
    }
  }

  def print(str: String): Unit = {
    ifEnabled() {
     scala.Predef.print(str)
    }
  }
}
