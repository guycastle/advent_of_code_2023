package days.day02

import days.DailyChallenge

object DayTwo extends DailyChallenge[Int]:

  override lazy val day: Int = 2

  private case class BagContents(gameId: Int, blues: Int = 0, reds: Int = 0, greens: Int = 0)

  private val parseLine: String => Option[BagContents] =
    case s"Game $gameId: $counts" => Some(counts.split(";").foldLeft(BagContents(gameId.toInt)) {
        case (bag, grabSample) => grabSample.split(",").map(_.strip).foldLeft(bag) {
            case (bag, s"$num blue") if num.toInt > bag.blues   => bag.copy(blues = num.toInt)
            case (bag, s"$num green") if num.toInt > bag.greens => bag.copy(greens = num.toInt)
            case (bag, s"$num red") if num.toInt > bag.reds     => bag.copy(reds = num.toInt)
            case (bag, _)                                       => bag
          }
      })
    case other                    =>
      println(s"Line matching failed on $other")
      None

  override def partOne(input: Seq[String]): Int = input
    .map(parseLine)
    .collect {
      case Some(bag) if bag.greens <= 13 && bag.reds <= 12 && bag.blues <= 14 => bag.gameId
    }
    .sum

  override def partTwo(input: Seq[String]): Int =
    input.map(parseLine).collect { case Some(bag) => bag.reds * bag.greens * bag.blues }.sum

  @main def run(): Unit = evaluate()

end DayTwo
