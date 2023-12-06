package days.day06

import days.BaseTest
import days.day06.DaySix.Race

class DaySixTest extends BaseTest:

  lazy val input: Seq[String] = Seq(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )

  lazy val partOneParsed: Seq[Race] = Seq(Race(7L, 9L), Race(15L, 40L), Race(30L, 200L))

  "On day six the challenge solution" must:
    "parse the input correctly for part one" in:
      DaySix.extractTimeAndDistance(DaySix.parseInputPartOne)(input) mustEqual partOneParsed

    "return the correct number of ways to beat the first race" in:
      DaySix.waysToBeatRecord(partOneParsed.head) mustBe 4L

    "calculate the correct score for all races in part one" in:
      DaySix.partOne(input) mustBe 288L

    "parse the input correctly for part two" in:
      DaySix.extractTimeAndDistance(DaySix.parseInputPartTwo)(input) mustBe Race(time = 71530L, distance = 940200L)

    "calculate the correct score for the race in part two" in:
      DaySix.partTwo(input) mustBe 71503L

end DaySixTest
