package year2022

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{Printer, Task}

class Day7 extends Task(2022, 7) with AnyWordSpecLike with Matchers {
  sealed trait FileSystemEntry {
    val name: String
    val parent: Option[Dir]
  }

  type FileSystem = Seq[FileSystemEntry]

  implicit class FileSystemOps(fs: FileSystem) {
    def getContentFor(dir: Dir): Seq[FileSystemEntry] = {
      fs.filter(_.parent.exists(_ == dir))
    }

    def calculateDirSizes(): Seq[DirWithSize] = {
      def calculateSubDirSize(fileSystem: FileSystem, contentToVisit: Seq[FileSystemEntry], actualSize: Int): Int = {
        contentToVisit.map {
          case file: File =>
            file.size
          case dir: Dir   =>
            val newContentToVisit = fileSystem.getContentFor(dir)
            actualSize + calculateSubDirSize(fileSystem, newContentToVisit, actualSize)
        }
      }.sum

      fs
        .filter(_.isInstanceOf[Dir])
        .map(dir => DirWithSize(dir.name, calculateSubDirSize(fs, Seq(dir), 0)))
    }

    def toPrettyString(rootDir: FileSystemEntry): String = {
      def subDirsToPrettyString(contentToVisit: Seq[FileSystemEntry], spaces: String): String = {
        contentToVisit.map {
          case file: File =>
            spaces + s"- ${file.name} (file, size=${file.size})\n"
          case dir: Dir   =>
            val newContentToVisit = fs.getContentFor(dir)
            spaces + s"- ${dir.name} (dir)\n" + subDirsToPrettyString(
              newContentToVisit,
              spaces + "  ",
            )
        }
      }.mkString("")

      subDirsToPrettyString(Seq(rootDir), "")
    }
  }

  case class Dir(name: String, parent: Option[Dir]) extends FileSystemEntry

  case class File(name: String, parent: Option[Dir], size: Int) extends FileSystemEntry

  case class DirWithSize(name: String, size: Int)

  def parseFileSystem(input: Seq[String]): FileSystem = {
    val rootDirInfo = s"dir ${input.head.split(" ")(2)}"

    val (_, fileSystem) = (Seq(rootDirInfo) ++ input).foldLeft((Seq.empty[Dir], Seq.empty[FileSystemEntry])) { case ((path, entities), command) =>
      command match {
        case "$ ls"             =>
          (path, entities)
        case "$ cd .."          =>
          (path.dropRight(1), entities)
        case s"$$ cd $dirName"  =>
          val actualDir   = Dir(dirName, path.lastOption)
          val newEntities = if (entities.isEmpty) Seq(actualDir) else entities
          (path :+ actualDir, newEntities)
        case s"dir $dirName"    =>
          val actualDir = Dir(dirName, path.lastOption)
          (path, entities :+ actualDir)
        case s"$size $fileName" =>
          val actualFile = File(fileName, path.last.some, size.toInt)
          (path, entities :+ actualFile)
        case other              =>
          throw new UnknownError(s"Unknown command: $other")
      }
    }

    fileSystem
  }

  def part1(input: Seq[String]): Int = {
    val fileSystem = parseFileSystem(input)

    val rootDir = {
      assert(fileSystem.head.isInstanceOf[Dir])
      assert(fileSystem.head.asInstanceOf[Dir].name == "/")
      fileSystem.head.asInstanceOf[Dir]
    }

    printer.printLine(fileSystem.toPrettyString(rootDir))

    val dirsWithSizes = fileSystem.calculateDirSizes()
    dirsWithSizes.filter(_.size < 100000).map(_.size).sum
  }

  def part2(input: Seq[String]): Int = {
    val fileSystem = parseFileSystem(input)

    val rootDir = {
      assert(fileSystem.head.isInstanceOf[Dir])
      assert(fileSystem.head.asInstanceOf[Dir].name == "/")
      fileSystem.head.asInstanceOf[Dir]
    }

    val dirsWithSizes = fileSystem.calculateDirSizes()

    val rootDirWithSize = {
      val rootDirSizeOpt = dirsWithSizes.find(_.name == rootDir.name)
      assert(rootDirSizeOpt.isDefined)
      rootDirSizeOpt.get
    }
    val remainingSpace  = 70000000 - rootDirWithSize.size

    val dirToDelete = dirsWithSizes
      .filter(_.size + remainingSpace > 30000000)
      .minBy(_.size)

    dirToDelete.size
  }

  "Part 1" should {
    "solve the example" in {
      printer = new Printer(true)

      val input = getExample()
      part1(input) shouldBe 95437
    }

    "solve the task" in {
      printer = new Printer(false)

      val input = getTask()
      part1(input) shouldBe 1334506
    }
  }

  "Part 2" should {
    "solve the example" in {
      val input = getExample()
      part2(input) shouldBe 24933642
    }

    "solve the task" in {
      val input = getTask()
      part2(input) shouldBe 7421137
    }
  }
}
