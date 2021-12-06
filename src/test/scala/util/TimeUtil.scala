package util

object TimeUtil {
  def formatMillis(ms: Long): String = {
    val millis  = ms                      % 1000
    val seconds = (ms / 1000)             % 60
    val minutes = (ms / (1000 * 60))      % 60
    val hours   = (ms / (1000 * 60 * 60)) % 24

    String.format("%02d:%02d:%02d.%03d", hours, minutes, seconds, millis)
  }

  def formatNanos(ns: Long): String = {
    val nanos   = ns                               % 1000000000L
    val seconds = (ns / 1000000000L)               % 60L
    val minutes = (ns / (1000000000L * 60L))       % 60L
    val hours   = (ns / (1000000000L * 60L * 60L)) % 24L

    String.format("%02d:%02d:%02d.%09d", hours, minutes, seconds, nanos)
  }
}
