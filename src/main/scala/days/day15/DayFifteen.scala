package days.day15

import days.DailyChallenge

object DayFifteen extends DailyChallenge[Int]:

  override lazy val day: Int = 15

  override def partOne(input: Seq[String]): Int = parseInput(input).map(_.aocHash).sum

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  extension (str: String)
    def aocHash: Int = str.foldLeft(0):
      case (hash, char) if char != '\n' => ((hash + char.toInt) * 17) % 256
      case (hash, _)                    => hash
  end extension

  private lazy val parseInput: Seq[String] => Seq[String] = lines => lines.flatMap(_.split(','))

end DayFifteen
