package days.day03

import days.BaseTest

class DayThreeTest extends BaseTest {

  "the schematic" must {

    lazy val engineSchematic = Seq(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )

    "be successfully scanned for part numbers and calculate their sum" in {
      DayThree.partOne(engineSchematic) mustBe 4361
    }

    "be successfully scanned for gears and calculate their power" in {
      DayThree.partTwo(engineSchematic) mustBe 467835
    }
  }

}
