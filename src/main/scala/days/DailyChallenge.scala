package days

import utils.InputReader

import scala.util.{Failure, Success, Try}

trait DailyChallenge[O]:

  lazy val day: Int

  def partOne(input: Seq[String]): O

  def partTwo(input: Seq[String]): O

  protected def evaluate(): Unit =
    val readResult = InputReader.readLines(s"inputs/day${String.format("%02d", day)}/input.txt")
    printResult(part = "one", readResult = readResult, processInput = partOne)
    printResult(part = "two", readResult = readResult, processInput = partTwo)

  private def printResult(part:         String,
                          readResult:   Try[Seq[String]],
                          start:        Long = System.currentTimeMillis,
                          processInput: Seq[String] => O): Unit =
    val output =
      for
        input  <- readResult
        output <- Try(processInput(input))
      yield output

    output match
      case Failure(ex)     => println(s"Error while processing input for part $part of day $day: ${ex.getMessage}")
      case Success(result) =>
        println(s"The result for part $part of day $day is: $result (took ${System.currentTimeMillis - start}ms)")

end DailyChallenge
