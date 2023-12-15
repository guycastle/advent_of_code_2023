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
      val pairs = DayEleven.parseInput(test, 1)
      pairs.size mustBe 36

    "correctly calculate the total distance between each pair in part one" in:
      DayEleven.partOne(test) mustBe 374

    "correctly calculate the total distance between each pair in part two" in:
      DayEleven.parseInput(test, 9).map(DayEleven.distanceBetweenGalaxies).sum mustBe 1030
      DayEleven.parseInput(test, 99).map(DayEleven.distanceBetweenGalaxies).sum mustBe 8410

end DayElevenTest
