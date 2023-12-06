package days.day02

import days.BaseTest

class DayTwoTest extends BaseTest:
  "The number of blocks in the bag" must:

    val input = Seq(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    "guess correctly how many may possibly match the maximum block counts" in:
      DayTwo.partOne(input) mustBe 8

    "calculate the power of the largest bag grabs" in:
      DayTwo.partTwo(input) mustBe 2286

end DayTwoTest
