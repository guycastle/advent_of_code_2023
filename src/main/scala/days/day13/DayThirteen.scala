package days.day13

import days.DailyChallenge

import scala.annotation.tailrec

object DayThirteen extends DailyChallenge[Long]:

  override lazy val day: Int = 13

  override def partOne(input: Seq[String]): Long = parseInput(input).map(reflectionIndex(_)).sum

  override def partTwo(input: Seq[String]): Long = parseInput(input).map(reflectionIndex(_, true)).sum

  @main def run(): Unit = evaluate()

  type Pattern = Seq[String]

  lazy val parseInput: Seq[String] => Seq[Pattern] = lines =>
    @tailrec
    def split(input: Seq[String], patterns: Seq[Pattern] = Seq.empty): Seq[Pattern] =
      if input.isEmpty then patterns
      else
        val (pattern, remnant) = input.span(!_.isBlank)
        split(input = remnant.drop(1), patterns = patterns :+ pattern)
    split(lines)

  private def reflectsAt(pattern: Pattern, withSmudge: Boolean = false): Option[Int] =
    lazy val mirrorsAt: Int => Boolean = idx =>
      val distance = math.min(pattern.size - idx, idx)
      if withSmudge then
        val smudges = (0 until distance).foldLeft(0L):
          case (smudges, i) =>
            val diff =
              for
                fwd <- pattern.lift(idx + i)
                bck <- pattern.lift(idx - (i + 1))
              yield fwd.zip(bck).count(_ != _)
            smudges + diff.getOrElse(0)
        smudges == 1
      else pattern.slice(idx, idx + distance) == pattern.slice(idx - distance, idx).reverse

    pattern.indices.collectFirst:
      // Don't bother checking the first index
      case idx if idx != 0 && mirrorsAt(idx) => idx

  def reflectionIndex(pattern: Pattern, withSmudge: Boolean = false): Int = reflectsAt(pattern, withSmudge)
    .map(_ * 100)
    .orElse(reflectsAt(pattern.transpose.map(_.mkString), withSmudge))
    .getOrElse(
      throw new IllegalArgumentException(
        s"Pattern has no reflection index: ${pattern.map(_.mkString).mkString("\n", "\n", "")}"
      )
    )

end DayThirteen
