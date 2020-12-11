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

  def isByrValid(input: Option[String]): Boolean = {
    input
      .map(_.toInt)
      .exists(i => 1920 <= i && i <= 2002)
  }

  def isIyrValid(input: Option[String]): Boolean = {
    input
      .map(_.toInt)
      .exists(i => 2010 <= i && i <= 2020)
  }

  def isEyrValid(input: Option[String]): Boolean = {
    input
      .map(_.toInt)
      .exists(i => 2020 <= i && i <= 2030)
  }

  def isHgtValid(input: Option[String]): Boolean = {
    input.exists(rawHeight => {
      rawHeight.substring(rawHeight.length - 2, rawHeight.length) match {
        case "cm" =>
          val height = rawHeight.dropRight(2).toInt
          150 <= height && height <= 193

        case "in" =>
          val height = rawHeight.dropRight(2).toInt
          59 <= height && height <= 76

        case _ =>
          false
      }
    })
  }

  def isHclValid(input: Option[String]): Boolean = {
    input.exists(_.matches("#[0-9a-f]{6}"))
  }

  def isEclValid(input: Option[String]): Boolean = {
    input.exists(ecl => {
      Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl)
    })
  }

  def isPidValid(input: Option[String]): Boolean = {
    input.exists(_.matches("[0-9]{9}"))
  }

  def checkData(data: Map[String, String]): Boolean = {
    isByrValid(data.get("byr")) &&
    isIyrValid(data.get("iyr")) &&
    isEyrValid(data.get("eyr")) &&
    isHgtValid(data.get("hgt")) &&
    isHclValid(data.get("hcl")) &&
    isEclValid(data.get("ecl")) &&
    isPidValid(data.get("pid"))
  }

  def checkPassport1(data: Map[String, String]): Boolean = {
    if (data.contains("cid")) {
      data.size == 8
    } else {
      data.size == 7
    }
  }

  def checkPassport2(data: Map[String, String]): Boolean = {
    if (data.contains("cid")) {
      val sizeOk = data.size == 8
      val dataOk = checkData(data)

      sizeOk && dataOk
    } else {
      val sizeOk = data.size == 7
      val dataOk = checkData(data)

      sizeOk && dataOk
    }
  }

  def part1(input: Seq[String]): Long = {
    input.mkString(" ")
      .split(" {2}")
      .count(rawData => checkPassport1(rawData.parseRawToMap()))
  }

  def part2(input: Seq[String]): Long = {
    input.mkString(" ")
      .split(" {2}")
      .count(rawData => checkPassport2(rawData.parseRawToMap()))
  }

  "Part 1" should {
    "solve the example" in {
      checkPassport1(
        """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
          |byr:1937 iyr:2017 cid:147 hgt:183cm""".parseTestToMap(),
      ) shouldBe true

      checkPassport1(
        """iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
          |hcl:#cfa07d byr:1929""".parseTestToMap(),
      ) shouldBe false

      checkPassport1(
        """hcl:#ae17e1 iyr:2013
          |eyr:2024
          |ecl:brn pid:760753108 byr:1931
          |hgt:179cm""".parseTestToMap(),
      ) shouldBe true

      checkPassport1(
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

  "Part 2" should {
    "solve the example" in {
      import cats.implicits._

      isByrValid("2002".some) shouldBe true
      isByrValid("2003".some) shouldBe false

      isHgtValid("60in".some) shouldBe true
      isHgtValid("190cm".some) shouldBe true
      isHgtValid("190in".some) shouldBe false
      isHgtValid("190".some) shouldBe false

      isHclValid("#123abc".some) shouldBe true
      isHclValid("#123abz".some) shouldBe false
      isHclValid("123abc".some) shouldBe false

      isEclValid("brn".some) shouldBe true
      isEclValid("wat".some) shouldBe false

      isPidValid("000000001".some) shouldBe true
      isPidValid("0123456789".some) shouldBe false

      checkPassport2(
        """eyr:1972 cid:100
          |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926""".parseTestToMap(),
      ) shouldBe false

      checkPassport2(
        """iyr:2019
          |hcl:#602927 eyr:1967 hgt:170cm
          |ecl:grn pid:012533040 byr:1946""".parseTestToMap(),
      ) shouldBe false

      checkPassport2(
        """hcl:dab227 iyr:2012
          |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277""".parseTestToMap(),
      ) shouldBe false

      checkPassport2(
        """hgt:59cm ecl:zzz
          |eyr:2038 hcl:74454a iyr:2023
          |pid:3556412378 byr:2007""".parseTestToMap(),
      ) shouldBe false

      checkPassport2(
        """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
          |hcl:#623a2f""".parseTestToMap(),
      ) shouldBe true

      checkPassport2(
        """eyr:2029 ecl:blu cid:129 byr:1989
          |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm""".parseTestToMap(),
      ) shouldBe true

      checkPassport2(
        """hcl:#888785
          |hgt:164cm byr:2001 iyr:2015 cid:88
          |pid:545766238 ecl:hzl
          |eyr:2022""".parseTestToMap(),
      ) shouldBe true

      checkPassport2(
        """iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".parseTestToMap(),
      ) shouldBe true
    }

    "solve the task" in {
      val input = InputReader.getTask(year, day)
      part2(input) shouldBe 186
    }
  }
}
