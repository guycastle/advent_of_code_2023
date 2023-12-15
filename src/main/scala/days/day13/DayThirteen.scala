package days.day13

import days.DailyChallenge

import scala.annotation.tailrec

object DayThirteen extends DailyChallenge[Long]:

  override lazy val day: Int = 13

  override def partOne(input: Seq[String]): Long = parseInput(input).map(reflectionIndex).sum

  override def partTwo(input: Seq[String]): Long = 0
  
  @main def run(): Unit = evaluate()

  type Pattern = Seq[String]

  lazy val parseInput: Seq[String] => Seq[Pattern] = lines =>
    @tailrec
    def split(input: Seq[String], patterns: Seq[Pattern] = Seq.empty): Seq[Pattern] =
      if input.isEmpty then patterns else
        val (pattern, remnant) = input.span(!_.isBlank)
        split(input = remnant.drop(1), patterns = patterns :+ pattern)
    split(lines)

  private lazy val reflectsAt: Pattern => Option[Int] = pattern =>
    lazy val mirrorsAt: Int => Boolean = idx =>
      val distance = math.min(pattern.size - idx, idx)
      pattern.slice(idx, idx + distance) == pattern.slice(idx - distance, idx).reverse

    pattern.indices.collectFirst:
      // Don't bother checking the first index  
      case idx if idx != 0 && mirrorsAt(idx) => idx
      
  lazy val reflectionIndex: Pattern => Int = pattern => 
    reflectsAt(pattern).map(_ * 100)
      .orElse(reflectsAt(pattern.transpose.map(_.mkString)))
      .getOrElse(throw new IllegalArgumentException(s"Pattern has no reflection index: ${pattern.map(_.mkString).mkString("\n", "\n", "")}"))

end DayThirteen

