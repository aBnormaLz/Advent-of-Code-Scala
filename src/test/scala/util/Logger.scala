package util

class Logger(loggingEnabled: Boolean) {
  def newLine(): Unit = {
    if (loggingEnabled) {
      println()
    }
  }

  def info(str: String): Unit = {
    if (loggingEnabled) {
      println(str)
    }
  }

  def append(str: String): Unit = {
    if (loggingEnabled) {
      print(str)
    }
  }
}
