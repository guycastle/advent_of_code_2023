package days.day01

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DayOneTest extends AnyWordSpec with Matchers {

  "extracting calibration values from input" must {

    "in part one calculate the correct output" in {

      val input: Seq[String] = Seq(
        "1abc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        "treb7uchet"
      )

      DayOne.partOne(input) mustBe 142

    }

    "in part two" must {

      "correctly calculate the example input" in {
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

      "take into account overlap" in {
        val input: Seq[String] = Seq(
          "twone", // 21
          "eighthree", // 83
          "sevenine", // 79
          "nineight", // 98
          "fiveight", // 58
          "threeight", // 38
          "oneight", // 18
        )

        DayOne.partTwo(input) mustBe (21 + 83 + 79 + 98 + 58 + 38 + 18)
      }
    }
  }
}
