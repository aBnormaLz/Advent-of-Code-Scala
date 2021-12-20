package year2021.day13

import cats.implicits._
import util.Ops.{IntCoordsOps, IntOps, MatrixOps}
import util._

object Util {
  case class Paper(dots: Seq[Seq[Int]]) {
    def foldOn(axis: String)(implicit printer: Printer): Paper = {
      axis match {
        case "x" =>
          printer.printLine(s"Folding along x=${dots.head.size / 2}")
          foldLeft()
        case "y" =>
          printer.printLine(s"Folding along y=${dots.size / 2}")
          foldUp()
      }
    }

    def foldUp(): Paper = {
      val folded = Paper(dots.transpose).foldLeft()

      Paper(folded.dots.transpose)
    }

    def foldLeft(): Paper = {
      val lineNum = dots.head.length / 2
      val newDots = for (y <- dots.indices) yield {
        val line = dots(y)
        for (x <- 0 until lineNum) yield {
          (line(x) + line(line.length - 1 - x)).butMax(1)
        }
      }

      Paper(newDots)
    }

    def dotCount(): Int = {
      dots.map(_.sum).sum
    }

    def print()(implicit printer: Printer): Unit = {
      printer.ifEnabled() {
        dots.nestedForeach { value =>
          if (value > 0) {
            printer.print("#")
          } else {
            printer.print(".")
          }
        } { _ =>
          printer.printLine()
        }
      }
    }

    def toPrettyString(): String = {
      dots.toPrettyString(value => {
        if (value > 0) {
          "#"
        } else {
          "."
        }
      })
    }
  }

  object Paper {
    def apply(input: Seq[String])(implicit dummyImplicit: DummyImplicit): Paper = {
      val dotCoords    = input.map {
        case s"$x,$y" => (x.toInt, y.toInt)
      }
      val (xMax, yMax) = dotCoords.getMaxes() |+| (1, 1)

      val emptyDots = Seq.fill(xMax)(Seq.fill(yMax)(0))

      val dots = dotCoords.foldLeft(emptyDots) {
        case (prevDots, nextDot) =>
          prevDots.updatedAt(nextDot, _ + 1)
      }.transpose

      this(dots)
    }
  }
}
