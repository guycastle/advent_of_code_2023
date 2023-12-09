package days.day09

import days.BaseTest
import days.day09.DayNine.Time

class DayNineTest extends BaseTest:

  lazy val test: Seq[String] = Seq(
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  )

  lazy val lineTest1: Seq[Int] = Seq(0, 3, 6, 9, 12, 15)
  lazy val lineTest2: Seq[Int] = Seq(1, 3, 6, 10, 15, 21)
  lazy val lineTest3: Seq[Int] = Seq(10, 13, 16, 21, 30, 45)
  lazy val lineTest4: Seq[Int] = Seq(-1, 0, -4, 7)

  "Day nine's solution" must:

    "parse the input correctly in part one" in:
      DayNine.parseLines(test).headOption.value mustBe lineTest1
      DayNine.parseLines(Seq("-1 0 -4 7")) mustBe Seq(lineTest4)

    "correctly predict the number in the sequence in either future or past" in:
      DayNine.predict(Time.Future, lineTest1) mustBe 18
      DayNine.predict(Time.Future, lineTest2) mustBe 28
      DayNine.predict(Time.Future, lineTest3) mustBe 68
      DayNine.predict(Time.Past, lineTest1) mustBe -3
      DayNine.predict(Time.Past, lineTest2) mustBe 0
      DayNine.predict(Time.Past, lineTest3) mustBe 5

    "correctly calculate the total for all input lines for part one" in:
      DayNine.partOne(test) mustBe 114
      DayNine.partTwo(test) mustBe 2

end DayNineTest
