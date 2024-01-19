package days.day15

import days.BaseTest

class DayFifteenTest extends BaseTest:

  lazy val input: Seq[String] = Seq("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

  "Day fifteen's solution" must:

    "correctly calculate the hash code of 'HASH'" in:
      import DayFifteen.aocHash
      "HASH".aocHash mustBe 52

    "correctly calculate the sum of the input's hashes" in:
      DayFifteen.partOne(input) mustBe 1320
