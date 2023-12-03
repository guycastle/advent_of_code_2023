package days

import utils.InputReader

import scala.util.{Failure, Success, Try}

trait DailyChallenge[O] {

  lazy val day: Int

  def partOne(input: Seq[String]): O

  def partTwo(input: Seq[String]): O

  protected def evaluate(): Unit =
    val readResult = InputReader.readLines(s"inputs/day${day.toString.padTo(2, '0').reverse}/input.txt")
    printResult("one", readResult, partOne)
    printResult("two", readResult, partTwo)

  private def printResult(part: String, readResult: Try[Seq[String]], processInput: Seq[String] => O): Unit =
    val output =
      for
        input  <- readResult
        output <- Try(processInput(input))
      yield output

    output match
      case Failure(ex)     => println(s"Error while processing input for part $part of day $day: ${ex.getMessage}")
      case Success(result) => println(s"The result for part $part of day $day is: $result")
}
