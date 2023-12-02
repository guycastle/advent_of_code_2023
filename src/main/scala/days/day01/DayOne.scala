package days.day01

import days.DailyChallenge

import scala.util.matching.Regex

object DayOne extends DailyChallenge[Int]:

  override lazy val day: Int = 1

  private lazy val calibrationValuePart1: String => Int = _.collect { case c if c.isDigit => c.toString.toInt } match
    case digits if digits.isEmpty => 0
    case digits                   => (digits.head * 10) + digits.last

  private lazy val digitMap = Map(
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9
  )

  // Take both keys and values from the digit map and compose a regular expression with lookahead
  private lazy val numberRegex: Regex = (digitMap.keys ++ digitMap.values.map(_.toString)).mkString("(?=(", "|", "))").r

  // if it's not in our map, it's an  actual digit
  private lazy val matchToInt: String => Int = s => digitMap.getOrElse(s, s.toInt)

  private lazy val calibrationValuePart2: String => Int = str =>
    numberRegex.findAllIn(str).matchData.toSeq.map(_.group(1)) match
      case digits if digits.isEmpty   => 0
      case digits if digits.size == 1 =>
        val digit = matchToInt(digits.head)
        (digit * 10) + digit
      case digits                     => (matchToInt(digits.head) * 10) + matchToInt(digits.last)

  override def partOne(input: Seq[String]): Int = input.map(calibrationValuePart1).sum

  override def partTwo(input: Seq[String]): Int = input.map(calibrationValuePart2).sum

  @main def run(): Unit = evaluate()

end DayOne
