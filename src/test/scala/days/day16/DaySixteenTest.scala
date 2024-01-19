package days.day16

import days.BaseTest

class DaySixteenTest extends BaseTest:

  lazy val input: Seq[String] = Seq(
    """.|...\....""",
    """|.-.\.....""",
    """.....|-...""",
    """........|.""",
    """..........""",
    """.........\""",
    """..../.\\..""",
    """.-.-/..|..""",
    """.|....-|.\""",
    """..//.|...."""
  )

  "Day sixteen's solution" must:

    "correctly navigate the contraption and count the energised tiles" in:
      DaySixteen.partOne(input) mustBe 46
