package year2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.Task

class Day8 extends Task(2022, 8) with AnyWordSpecLike with Matchers {
  def parseTreeMap(input: Seq[String]): Seq[Seq[Int]] = {
    input.map(_.map(_.toInt - '0'))
  }

  def countEdgeTrees(treeMap: Seq[Seq[Int]]): Int = {
    treeMap.size * 2 + (treeMap.head.size - 2) * 2
  }

  def countVisibleInteriorTrees(treeMap: Seq[Seq[Int]]): Int = {
    (for {
      x <- 1 until treeMap.size - 1
      y <- 1 until treeMap.size - 1
    } yield {
      getTreesInDirections(treeMap, x, y).exists(_.forall(_ < treeMap(x)(y)))
    }).count(_ == true)
  }

  def getTreesInDirections(treeMap: Seq[Seq[Int]], x: Int, y: Int): Seq[Seq[Int]] = {
    Seq(
      treeMap.map(_(y)).slice(0, x).reverse,
      treeMap(x).slice(y + 1, treeMap(x).size),
      treeMap.map(_(y)).slice(x + 1, treeMap.map(_(y)).size),
      treeMap(x).slice(0, y).reverse,
    )
  }

  def part1(input: Seq[String]): Int = {
    val treeMap = parseTreeMap(input)
    countEdgeTrees(treeMap) + countVisibleInteriorTrees(treeMap)
  }

  def getScenicValue(treeMap: Seq[Seq[Int]], x: Int, y: Int, trees: Seq[Int]): Int = {
    val numberOfTrees = trees.indexWhere(_ >= treeMap(x)(y))
    if (numberOfTrees < 0) {
      trees.size
    } else {
      numberOfTrees + 1
    }
  }

  def part2(input: Seq[String]): Int = {
    val treeMap = parseTreeMap(input)

    (for {
      x <- 1 until treeMap.size - 1
      y <- 1 until treeMap.size - 1
    } yield {
      getTreesInDirections(treeMap, x, y)
        .map(getScenicValue(treeMap, x, y, _))
        .product
    }).max
  }

  "Part 1" should {
    "solve the example" in {
      val input = getExample()
      part1(input) shouldBe 21
    }

    "solve the task" in {
      val input = getTask()
      part1(input) shouldBe 1703
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 8
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 496650
    }
  }
}
