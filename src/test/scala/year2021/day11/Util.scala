package year2021.day11

import util.Ops.{IntCoordOps, MatrixOps}
import util.Printer

object Util {
  type OctopiEnergies = Seq[Seq[Int]]
  type Pos            = (Int, Int)

  implicit class OctopiOps(octopiEnergies: OctopiEnergies) {
    def getFlashPoses(): Set[Pos] = {
      val xMax = octopiEnergies.size - 1
      val yMax = octopiEnergies.head.size - 1
      (for (y <- 0 to yMax) yield {
        for (x <- 0 to xMax) yield {
          if (octopiEnergies(x)(y) > 9) {
            Some(x, y)
          } else {
            None
          }
        }
      }).flatMap(_.collect {
        case Some(pos) => pos
      }).toSet
    }

    def flash(pos: Pos, flashed: Set[Pos])(implicit printer: Printer): OctopiEnergies = {
      octopiEnergies.updatedAt(pos, 0)
        .updatedAt(pos.up(), newValue(pos.up(), _, flashed))
        .updatedAt(pos.down(), newValue(pos.down(), _, flashed))
        .updatedAt(pos.left(), newValue(pos.left(), _, flashed))
        .updatedAt(pos.right(), newValue(pos.right(), _, flashed))
        .updatedAt(pos.up().left(), newValue(pos.up().left(), _, flashed))
        .updatedAt(pos.up().right(), newValue(pos.up().right(), _, flashed))
        .updatedAt(pos.down().left(), newValue(pos.down().left(), _, flashed))
        .updatedAt(pos.down().right(), newValue(pos.down().right(), _, flashed))
    }

    def newValue(pos: Pos, previousValue: Int, flashed: Set[Pos]): Int = {
      if (flashed.contains(pos)) {
        0
      } else {
        previousValue + 1
      }
    }

    def incrementAllBy(increment: Int): OctopiEnergies = {
      octopiEnergies.map(_.map(_ + increment))
    }

    def isSynced(): Boolean = {
      val xMax = octopiEnergies.size - 1
      val yMax = octopiEnergies.head.size - 1
      (for (y <- 0 to yMax) yield {
        for (x <- 0 to xMax) yield {
          if (octopiEnergies(x)(y) == 0) {
            true
          } else {
            false
          }
        }
      }).flatten
        .forall(_ == true)
    }

    def printEnergies()(implicit printer: Printer): Unit = {
      printer.ifEnabled() {
        printer.printLine(
          octopiEnergies.toPrettyString(value => {
            if (value > 9) {
              "x"
            } else if (value == 0) {
              " "
            } else {
              value.toString
            }
          }),
        )
      }
    }
  }
}
