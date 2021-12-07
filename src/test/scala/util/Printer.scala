package util

class Printer(printingEnabled: Boolean) {
  def ifEnabled()(callback: => Unit): Unit = {
    if (printingEnabled) {
      callback
    }
  }

  def print(str: String): Unit = {
    ifEnabled() {
      scala.Predef.print(str)
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

  def printSoftLine(): Unit = {
    printLine("--------------------")
  }

  def printHardLine(): Unit = {
    printLine("====================")
  }

  def printTitle(str: String): Unit = {
    printLine()
    printLine("=" * (str.length + 8))
    printLine(s"=== $str ===")
    printLine("=" * (str.length + 8))
    printLine()
  }
}
