package days.day07

import days.BaseTest
import days.day07.DaySeven.HandType.{FiveOfAKind, FourOfAKind, FullHouse, OnePair, ThreeOfAKind}

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
      val hand = input.headOption.map(DaySeven.parseInput(1, _)).value
      hand.bid mustBe 765
      hand.cards mustBe Seq(1, 0, 8, 1, 11)
      hand.handType mustBe OnePair

    "parse the hands correctly in part two" in:
      val hand = input.lift(1).map(DaySeven.parseInput(2, _)).value
      hand.bid mustBe 684
      hand.cards mustBe Seq(9, 4, 4, 0, 4)
      hand.handType mustBe FourOfAKind

    "successfully parse all hand types in part two using jokers" in:
      val jokerInputs = Seq(
        "2222J 0",
        "222JJ 0",
        "22JJJ 0",
        "333J2 0",
        "JJJ43 0",
        "2233J 0",
        "JJ443 0",
        "22J45 0",
        "JJ345 0",
        "2345J 0",
        "JJJJ2 0"
      ).map(DaySeven.parseInput(2, _))

      jokerInputs match
        case Seq(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth, eleventh) =>
          first.handType mustBe FiveOfAKind
          second.handType mustBe FiveOfAKind
          third.handType mustBe FiveOfAKind
          fourth.handType mustBe FourOfAKind
          fifth.handType mustBe FourOfAKind
          sixth.handType mustBe FullHouse
          seventh.handType mustBe FourOfAKind
          eighth.handType mustBe ThreeOfAKind
          ninth.handType mustBe ThreeOfAKind
          tenth.handType mustBe OnePair
          eleventh.handType mustBe FiveOfAKind
        case other                                                                                    => fail("Parsed input mismatch")

    "calculate the correct winnings for part one" in:
      DaySeven.partOne(input) mustBe 6440

    "calculate the correct winnings for part two" in:
      DaySeven.partTwo(input) mustBe 5905

end DaySevenTest
