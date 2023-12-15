package days.day12

import days.DailyChallenge

object DayTwelve extends DailyChallenge[Int]:

  override lazy val day: Int = 12

  override def partOne(input: Seq[String]): Int = 0

  override def partTwo(input: Seq[String]): Int = 0

  @main def run() = evaluate()
  
  enum Status:
    case Operational, Damaged, Unknown
  end Status

  case class Record(statuses: Seq[Status], contiguousDamaged: Seq[Int])

  lazy val parseInputLine: String => Record = line =>
    val (statuses, cont) = line.span(!_.isWhitespace)
    Record(
      statuses = statuses.map:
        case '?' => Status.Unknown
        case '#' => Status.Damaged
        case '.' => Status.Operational
      , contiguousDamaged = cont.drop(1).withFilter(_ != ',').map(_.toString.toInt)
    )

end DayTwelve
