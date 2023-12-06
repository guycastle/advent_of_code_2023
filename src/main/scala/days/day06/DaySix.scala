package days.day06

import days.DailyChallenge

import scala.annotation.{tailrec, unused}
import scala.util.matching.Regex

object DaySix extends DailyChallenge[Long]:

  override lazy val day: Int = 6

  override def partOne(input: Seq[String]): Long =
    extractTimeAndDistance[Seq[Race]](parseInputPartOne)(input).map(waysToBeatRecord).product

  override def partTwo(input: Seq[String]): Long = waysToBeatRecord(extractTimeAndDistance(parseInputPartTwo)(input))

  @main def run(): Unit = evaluate()

  lazy val numberRegex: Regex = "[0-9]+".r

  case class Race(time: Long, distance: Long)

  lazy val waysToBeatRecord: Race => Long = race =>
    @tailrec
    def calculateMinMax(speed: Long, timeLeft: Long, increment: Int): Long =
      if speed * timeLeft > race.distance then speed
      else calculateMinMax(speed + increment, timeLeft - increment, increment)

    val min = calculateMinMax(speed = 1L, timeLeft = race.time - 1, increment = 1)
    val max = calculateMinMax(speed = race.time - 1, timeLeft = 1, increment = -1)
    (min to max).length

  @unused
  private lazy val firstAttemptAtCalculatingWaysToBeat: Race => Long = race =>
    (1L to race.time).foldLeft(0L):
      case (waysToBeat, time) if (race.time - time) * time > race.distance => waysToBeat + 1
      case (waysToBeat, _)                                                 => waysToBeat

  def extractTimeAndDistance[R](toRace: (String, String) => R): Seq[String] => R =
    case Seq(time, distance) => toRace(time, distance)
    case other               => throw new IllegalArgumentException(s"Invalid number of lines in input $other")

  lazy val parseInputPartTwo: (String, String) => Race =
    (time, distance) => Race(time = time.filter(_.isDigit).toLong, distance = distance.filter(_.isDigit).toLong)

  lazy val parseInputPartOne: (String, String) => Seq[Race] = (time, distance) =>
    numberRegex.findAllIn(time).zip(numberRegex.findAllIn(distance)).toSeq.map((t, d) => Race(t.toLong, d.toLong))

end DaySix
