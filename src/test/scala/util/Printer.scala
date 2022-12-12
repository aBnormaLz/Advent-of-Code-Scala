package util

class Printer(printingEnabled: Boolean) {
  def ifEnabled()(callback: => Unit): Unit = {
    if (printingEnabled) {
      callback
    }
  }

  def print(str: => String): Unit = {
    ifEnabled() {
      scala.Predef.print(str)
    }
  }

  def printLine(): Unit = {
    ifEnabled() {
      scala.Predef.println()
    }
  }

  def printLine(str: => String): Unit = {
    ifEnabled() {
      scala.Predef.println(str)
    }
  }

  def printSoftLine(): Unit = {
    ifEnabled() {
      printLine("--------------------")
    }
  }

  def printHardLine(): Unit = {
    ifEnabled() {
      printLine("====================")
    }
  }

  def printTitle(str: String): Unit = {
    ifEnabled() {
      printLine()
      printLine("=" * (str.length + 8))
      printLine(s"=== $str ===")
      printLine("=" * (str.length + 8))
      printLine()
    }
  }

  def printInfo(infos: (String, String)*): Unit = {
    ifEnabled() {
      infos.foreach {
        case (key, info) => printLine(s"$key$info")
      }
    }
  }
}
