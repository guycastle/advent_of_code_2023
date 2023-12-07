package days.day07

import days.BaseTest
import days.day07.DaySeven.HandType.OnePair

class DaySevenTest extends BaseTest:

  "The day seven solution" must:
    lazy val input: Seq[String] = Seq(
      "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483"
    )

    "parse the hands correctly in part one" in:
      val hand = input.headOption.map(DaySeven.parseInput).value
      hand.bid mustBe 765
      hand.cards mustBe Seq(1, 0, 8, 1, 11)
      hand.handType mustBe OnePair

    "calculate the correct winnings for part one" in:
      DaySeven.partOne(input) mustBe 6440

end DaySevenTest
