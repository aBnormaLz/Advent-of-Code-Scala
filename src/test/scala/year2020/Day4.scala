package year2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import util.{InputReader, Task}

class Day4 extends Task(2020, 4) with AnyWordSpecLike with Matchers {
  implicit class StringParser(string: String) {
    def parseTestToMap(): Map[String, String] = {
      string
        .stripMargin
        .replace("\n", " ")
        .parseRawToMap()
    }

    def parseRawToMap(): Map[String, String] = {
      string
        .split(" ")
        .map(_.split(":"))
        .map(elem => elem.head -> elem(1))
        .toMap
    }
  }

  def checkPassport(data: Map[String, String]): Boolean = {
    if (data.contains("cid")) {
      data.size == 8
    } else {
      data.size == 7
    }
  }

  def part1(input: Seq[String]): Long = {
    input.mkString(" ")
      .split(" {2}")
      .count(rawData => checkPassport(rawData.parseRawToMap()))
  }

  "Part 1" should {
    "solve the example" in {
      checkPassport(
        """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
          |byr:1937 iyr:2017 cid:147 hgt:183cm""".parseTestToMap(),
      ) shouldBe true

      checkPassport(
        """iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
          |hcl:#cfa07d byr:1929""".parseTestToMap(),
      ) shouldBe false

      checkPassport(
        """hcl:#ae17e1 iyr:2013
          |eyr:2024
          |ecl:brn pid:760753108 byr:1931
          |hgt:179cm""".parseTestToMap(),
      ) shouldBe true

      checkPassport(
        """hcl:#cfa07d eyr:2025 pid:166559648
          |iyr:2011 ecl:brn hgt:59in""".parseTestToMap(),
      ) shouldBe false

      val input = InputReader.getExample(year, day)
      part1(input) shouldBe 2
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part1(input) shouldBe 242
    }
  }
}
