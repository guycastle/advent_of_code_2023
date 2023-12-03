package days.day03

import days.DailyChallenge

import scala.annotation.tailrec
import scala.util.matching.Regex

object DayThree extends DailyChallenge[Int] {

  override lazy val day: Int = 3

  private case class LineScan(symbolIndices: Seq[Int], partsByIndex: Map[Range, Int])

  private case class ScanResult(parts: Seq[Int] = Seq.empty)

  private case class GearPower(powers: Seq[Int] = Seq.empty)

  private lazy val symbols: Seq[Char] = Seq('=', '&', '#', '+', '$', '/', '*', '%', '-', '@')

  private lazy val symbolsRegex: Regex = symbols.mkString("""(\""", """|\""", ")").r

  private lazy val gearRegex: Regex = "\\*".r

  private lazy val numberRegex: Regex = "[0-9]+".r

  private lazy val adjacentIndices: Int => Range = {
    case 0 => 0 to 1
    case i => i - 1 to i + 1
  }

  private lazy val lineScanner: String => LineScan = line =>
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
                                           result:   ScanResult       = ScanResult()): ScanResult = current match
    case None       => result
    case Some(line) =>
      val nonPartIndices = (
        line.symbolIndices ++
          previous.map(_.symbolIndices).getOrElse(Seq.empty) ++
          next.map(_.symbolIndices).getOrElse(Seq.empty)
      ).distinct.sorted

      processRecursivelyForPartOne(
        lines    = lines.drop(1),
        previous = current,
        current  = next,
        next     = lines.headOption.map(lineScanner),
        result   = result.copy(parts = result.parts ++ line.partsByIndex.collect {
          case (range, part) if (range.start until range.end).intersect(nonPartIndices).nonEmpty => part
        })
      )

  private def symbolIndexMatchingParts(symbolIndices: Range, parts: Map[Range, Int]): Seq[Int] =
    parts.toSeq.collect {
      case (range, number) if range.intersect(symbolIndices).nonEmpty => number
    }

  @tailrec
  private def processRecursivelyForPartTwo(lines:    Seq[String],
                                           previous: Option[LineScan] = None,
                                           current:  Option[LineScan] = None,
                                           next:     Option[LineScan] = None,
                                           result:   GearPower        = GearPower()): GearPower = current match
    case None       => result
    case Some(line) =>
      val gearNumbers = line.symbolIndices.map(adjacentIndices)
        .map(gearIndices =>
          symbolIndexMatchingParts(gearIndices, line.partsByIndex) ++
          previous.map(ln => symbolIndexMatchingParts(gearIndices, ln.partsByIndex)).getOrElse(Seq.empty) ++
          next.map(ln => symbolIndexMatchingParts(gearIndices, ln.partsByIndex)).getOrElse(Seq.empty)
        ).filter(_.size == 2)

      processRecursivelyForPartTwo(
        lines    = lines.drop(1),
        previous = current,
        current  = next,
        next     = lines.headOption.map(gearScanner),
        result   = result.copy(powers = result.powers ++ gearNumbers.map(_.product))
      )

  private lazy val processInputForPartOne: Seq[String] => ScanResult = lines =>
    processRecursivelyForPartOne(
      lines   = lines.drop(2),
      current = lines.headOption.map(lineScanner),
      next    = lines.lift(1).map(lineScanner)
    )

  private lazy val processInputForPartTwo: Seq[String] => GearPower = lines =>
    processRecursivelyForPartTwo(
      lines   = lines.drop(2),
      current = lines.headOption.map(gearScanner),
      next    = lines.lift(1).map(gearScanner)
    )

  override def partOne(input: Seq[String]): Int = processInputForPartOne(input).parts.sum

  override def partTwo(input: Seq[String]): Int = processInputForPartTwo(input).powers.sum

  @main def run(): Unit = evaluate()
}
