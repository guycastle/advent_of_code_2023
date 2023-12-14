package days.day11

import days.BaseTest

class DayElevenTest extends BaseTest:

  lazy val test: Seq[String] = Seq(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  "Day eleven's solution" must:

    "correctly parse the input and pair galaxies" in:
      val pairs = DayEleven.parseInput(test)
      pairs.size mustBe 36

    "correctly calculate the total distance between each pair" in:
      DayEleven.partOne(test) mustBe 374
