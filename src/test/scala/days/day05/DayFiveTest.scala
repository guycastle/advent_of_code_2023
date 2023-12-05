package days.day05

import days.BaseTest
import days.day05.DayFive.RangeMap

import scala.collection.immutable.NumericRange

class DayFiveTest extends BaseTest {

  lazy val input: Seq[String] = Seq(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  )

  "The almanac for day five" must {
    "be successfully parsed in part one" in {
      val almanac = DayFive.almanacParser(input, DayFive.seedListParser).value
      almanac.seeds mustBe Seq(79, 14, 55, 13)
      almanac.seedsToSoils mustBe Seq(
        RangeMap(98, 50, 2),
        RangeMap(50, 52, 48)
      )
    }

    "be successfully parsed in part two" in {
      val expected: Set[NumericRange[Long]] = Set((79L until 93L), (55L until 68L))
      DayFive.almanacParser(input, DayFive.seedRangeParser).value.seeds.toSet mustBe expected
    }

    "calculate the correct closest location in part one" in {
      DayFive.partOne(input) mustBe 35
    }

    "calculate the correct closest location in part two" in {
      DayFive.partTwo(input) mustBe 46
    }
  }

}
