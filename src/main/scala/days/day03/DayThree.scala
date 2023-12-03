package days.day03

import days.DailyChallenge

import scala.annotation.tailrec
import scala.util.matching.Regex

object DayThree extends DailyChallenge[Int] {

  override lazy val day: Int = 3

  private case class LineScan(symbolIndices: Seq[Int], partsByIndex: Map[Range, Int])

  private lazy val symbols: Seq[Char] = Seq('=', '&', '#', '+', '$', '/', '*', '%', '-', '@')

  private lazy val symbolsRegex: Regex = symbols.mkString("""(\""", """|\""", ")").r

  private lazy val gearRegex: Regex = "\\*".r

  private lazy val numberRegex: Regex = "[0-9]+".r

  private lazy val adjacentIndices: Int => Range = {
    case 0 => 0 to 1
    case i => i - 1 to i + 1
  }

  private lazy val symbolScanner: String => LineScan = line =>
    LineScan(
      symbolIndices = symbolsRegex.findAllIn(line).matchData.map(_.start).toSeq.flatMap(adjacentIndices),
      partsByIndex  =
        numberRegex.findAllIn(line).matchData.map(num => (num.start until num.end) -> num.matched.toInt).toMap
    )

  private lazy val gearScanner: String => LineScan = line =>
    LineScan(
      symbolIndices = gearRegex.findAllIn(line).matchData.map(_.start).toSeq,
      partsByIndex  =
        numberRegex.findAllIn(line).matchData.map(num => (num.start until num.end) -> num.matched.toInt).toMap
    )

  @tailrec
  private def processRecursivelyForPartOne(lines:    Seq[String],
                                           previous: Option[LineScan] = None,
                                           current:  Option[LineScan] = None,
                                           next:     Option[LineScan] = None,
                                           result:   Int              = 0): Int = current match
    case None       => result
    case Some(line) =>
      val partNumberIndices = (
        line.symbolIndices ++
          previous.map(_.symbolIndices).getOrElse(Seq.empty) ++
          next.map(_.symbolIndices).getOrElse(Seq.empty)
      ).distinct.sorted

      processRecursivelyForPartOne(
        lines    = lines.drop(1),
        previous = current,
        current  = next,
        next     = lines.headOption.map(symbolScanner),
        result   = result + line.partsByIndex.collect {
          case (range, part) if (range.start until range.end).intersect(partNumberIndices).nonEmpty => part
        }.sum
      )

  private def symbolIndexMatchingParts(symbolIndices: Range, parts: Map[Range, Int]): Seq[Int] = parts.toSeq.collect {
    case (range, number) if range.intersect(symbolIndices).nonEmpty => number
  }

  @tailrec
  private def processRecursivelyForPartTwo(lines:    Seq[String],
                                           previous: Option[LineScan] = None,
                                           current:  Option[LineScan] = None,
                                           next:     Option[LineScan] = None,
                                           result:   Int              = 0): Int = current match
    case None       => result
    case Some(line) =>
      val gearNumbers = line.symbolIndices
        .map(adjacentIndices)
        .map(gearIndices =>
          symbolIndexMatchingParts(gearIndices, line.partsByIndex) ++
            previous.map(ln => symbolIndexMatchingParts(gearIndices, ln.partsByIndex)).getOrElse(Seq.empty) ++
            next.map(ln => symbolIndexMatchingParts(gearIndices, ln.partsByIndex)).getOrElse(Seq.empty)
        )
        .filter(_.size == 2)

      processRecursivelyForPartTwo(
        lines    = lines.drop(1),
        previous = current,
        current  = next,
        next     = lines.headOption.map(gearScanner),
        result   = result + gearNumbers.map(_.product).sum
      )

  override def partOne(input: Seq[String]): Int = processRecursivelyForPartOne(
    lines = input.drop(2),
    current = input.headOption.map(symbolScanner),
    next = input.lift(1).map(symbolScanner)
  )

  override def partTwo(input: Seq[String]): Int = processRecursivelyForPartTwo(
    lines = input.drop(2),
    current = input.headOption.map(gearScanner),
    next = input.lift(1).map(gearScanner)
  )

  @main def run(): Unit = evaluate()
}
