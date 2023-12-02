package days.day01

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DayOneTest extends AnyWordSpec with Matchers {

  "extracting calibration values from input" should {

    "in part one" in {

      val input: Seq[String] = Seq(
        "1abc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        "treb7uchet"
      )

      DayOne.partOne(input) mustBe 142

    }

    "in part two" in {
      val input: Seq[String] = Seq(
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen"
      )

      DayOne.partTwo(input) mustBe 281
    }
  }

}
