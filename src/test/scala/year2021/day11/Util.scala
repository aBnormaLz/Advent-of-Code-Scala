package year2021.day11

import util.Ops.{IntCoordOps, IntCoordValuesOps}
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
      octopiEnergies.setTo(pos, 0)
        .increment(pos.up(), flashed)
        .increment(pos.down(), flashed)
        .increment(pos.left(), flashed)
        .increment(pos.right(), flashed)
        .increment(pos.up().left(), flashed)
        .increment(pos.up().right(), flashed)
        .increment(pos.down().left(), flashed)
        .increment(pos.down().right(), flashed)
    }

    def setTo(pos: Pos, value: Int): OctopiEnergies = {
      octopiEnergies
        .updated(pos._1, octopiEnergies(pos._1).updated(pos._2, value))
    }

    def increment(pos: Pos, flashed: Set[Pos]): OctopiEnergies = {
      if (!octopiEnergies.isDefinedAt(pos)) {
        octopiEnergies
      } else {
        val previousValue = octopiEnergies.at(pos)
        val value         =
          if (flashed.contains(pos)) {
            0
          } else {
            previousValue + 1
          }

        if (octopiEnergies.isDefinedAt(pos)) {
          octopiEnergies
            .updated(pos._1, octopiEnergies(pos._1).updated(pos._2, value))
        } else {
          octopiEnergies
        }
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
        printer.printLine(toPrettyString())
      }
    }

    def toPrettyString(): String = {
      val xMax = octopiEnergies.size - 1
      val yMax = octopiEnergies.head.size - 1
      (for (y <- 0 to yMax) yield {
        (for (x <- 0 to xMax) yield {
          val value = octopiEnergies(x)(y)
          if (value > 9) {
            "x"
          } else if (value == 0) {
            " "
          } else {
            value
          }
        }).mkString

      }).mkString("\n")
    }
  }
}
