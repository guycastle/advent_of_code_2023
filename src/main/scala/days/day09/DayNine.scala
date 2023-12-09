package days.day09

import days.DailyChallenge

import scala.annotation.tailrec

object DayNine extends DailyChallenge[Int]:

  override lazy val day: Int = 9

  override def partOne(input: Seq[String]): Int = parseLines(input).map(predict(Time.Future, _)).sum

  override def partTwo(input: Seq[String]): Int = parseLines(input).map(predict(Time.Past, _)).sum

  @main def run(): Unit = evaluate()

  lazy val parseLines: Seq[String] => Seq[HistoricalData] = _.withFilter(!_.isBlank).map(_.split(" ").map(_.toInt).toSeq)

  enum Time(val operator: (Int, Int) => Int):
    case Past   extends Time(operator = (a, b) => b - a)
    case Future extends Time(operator = _ + _)
  end Time

  type HistoricalData = Seq[Int]
  extension (data: HistoricalData)
    def nextIn: Time => Option[Int] =
      case Time.Future => data.lastOption
      case Time.Past   => data.headOption
  end extension

  @tailrec
  def calculate(line: HistoricalData, sequences: Seq[HistoricalData] = Seq.empty): Seq[HistoricalData] =
    val next = line
      .sliding(2)
      .toSeq
      .collect:
        case Seq(a, b) => b - a
    if next.forall(_ == 0) then sequences
    else
      calculate(
        line      = next,
        sequences = (next.headOption ++ next.lastOption).toSeq +: sequences
      )

  def predict(direction: Time, from: HistoricalData): Int = direction.operator(
    calculate(from).flatMap(_.nextIn(direction)).reduce(direction.operator),
    from.nextIn(direction).getOrElse(0)
  )

end DayNine
