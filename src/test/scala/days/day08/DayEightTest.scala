package days.day08

import days.BaseTest
import days.day08.DayEight.{MapOfTheDesert, Turn}

class DayEightTest extends BaseTest:

  lazy val test1: Seq[String] = Seq(
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  lazy val test2: Seq[String] = Seq(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  lazy val test3: Seq[String] = Seq(
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  )

  "Day eight's solution" must:

    "parse the input correctly" in:
      DayEight.parseInput(test2) mustEqual MapOfTheDesert(directions = "LLR",
                                                          turns      = Map(
                                                            "AAA" -> Turn(left = "BBB", right = "BBB"),
                                                            "BBB" -> Turn(left = "AAA", right = "ZZZ"),
                                                            "ZZZ" -> Turn(left = "ZZZ", right = "ZZZ")
                                                          )
                                                         )

    "correctly determine the required number of steps in part one" in:
      DayEight.partOne(test1) mustBe 2

    "correctly iterate the directions again if destination is not reached during the first iteration" in:
      DayEight.partOne(test2) mustBe 6

    "correctly determine the required number of steps in part two" in:
      DayEight.partTwo(test3) mustBe 6

    "calculate the lowest common multiplier correctly" in:
      DayEight.lowestCommonMultiplier(Seq(8, 9, 21)) mustBe 504

end DayEightTest
