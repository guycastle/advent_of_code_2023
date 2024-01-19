package days.day15

import days.BaseTest

import DayFifteen.*

class DayFifteenTest extends BaseTest:

  lazy val input: Seq[String] = Seq("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

  "Day fifteen's solution" must:

    "correctly calculate the hash code of 'HASH'" in:
      "HASH".aocHash mustBe 52

    "correctly calculate the sum of the input's hashes" in:
      DayFifteen.partOne(input) mustBe 1320

    "correctly parse the input to a step" in:
      "rn=1".toStep mustBe Step(label = "rn", box = 0, focalLength = Some(1))
      "cm-".toStep mustBe Step(label = "cm", box = 0, focalLength = None)
      "ot=5".toStep mustBe Step(label = "ot", box = 3, focalLength = Some(5))

    "correctly calculate the sum of the focusing power for all boxes" in:
      DayFifteen.partTwo(input) mustBe 145
