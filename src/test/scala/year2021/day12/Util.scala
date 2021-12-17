package year2021.day12

object Util {
  case class Connection(_1: String, _2: String)

  object Connection {
    def apply(inputLine: String): Connection = {
      val split = inputLine.split("-")
      this(split(0), split(1))
    }
  }

  implicit class ConnectionOps(connection: Connection) {
    def anyEndpointMatches(node: String): Boolean = {
      node == connection._1 || node == connection._2
    }
  }

  case class CaveSystem(connections: Seq[Connection]) {
    def getNeighboursOf(node: String): Seq[String] = {
      connections
        .filter(connection => connection.anyEndpointMatches(node))
        .flatMap(connection => Seq(connection._1, connection._2))
        .filter(_ != node)
    }
  }
}
