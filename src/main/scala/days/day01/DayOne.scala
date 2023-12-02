package days.day01

import utils.InputReader

import scala.util.matching.Regex
import scala.util.{Failure, Success}

object DayOne:

  lazy private val calibrationValuePart1: String => Int =
    _.collect { case c if c.isDigit => c.toString.toInt } match
      case digits if digits.isEmpty => 0
      case digits => (digits.head * 10) + digits.last

  lazy private val digitMap = Map(
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
  )

  // Take both keys and values from the digit map and compose a regular expression with lookahead
  lazy private val numberRegex: Regex =
    (digitMap.keys ++ digitMap.values.map(_.toString)).mkString("(?=(", "|", "))").r

  lazy private val matchToInt: String => Int = s => digitMap.getOrElse(s, s.toInt) // if it's not in our map, it's an actual diigit

  lazy private val calibrationValuePart2: String => Int = str =>
    numberRegex.findAllIn(str).matchData.toSeq.map(_.group(1)) match
      case digits if digits.isEmpty => 0
      case digits if digits.size == 1 =>
        val digit = matchToInt(digits.head)
        (digit * 10) + digit
      case digits =>
        (matchToInt(digits.head) * 10) + matchToInt(digits.last)

  def partOne(input: Seq[String]): Int =
    input.map(calibrationValuePart1).sum


  def partTwo(input: Seq[String]): Int =
    input.map(calibrationValuePart2).sum


  @main def run(): Unit =
    InputReader.readLines("inputs/day01/input.txt").map(partOne) match
      case Failure(ex) => println(ex.getMessage)
      case Success(result) => println(result)

    InputReader.readLines("inputs/day01/input.txt").map(partTwo) match
      case Failure(ex) => println(ex.getMessage)
      case Success(result) => println(result)

end DayOne
