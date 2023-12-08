package days.day08

import days.DailyChallenge

import scala.annotation.tailrec

object DayEight extends DailyChallenge[Int]:

  override lazy val day: Int = 8

  override def partOne(input: Seq[String]): Int =
    stepsToNavigateAcrossDesert(map = parseInput(input), startAt = "AAA", endAt = "ZZZ")

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  private def stepsToNavigateAcrossDesert(map: MapOfTheDesert, startAt: String, endAt: String): Int =
    @tailrec
    def navigate(directions: Seq[Char], turn: Turn, turnCount: Int = 0): Int = directions.headOption match
      case Some(direction) =>
        val next = turn.to(direction)
        if next == endAt then turnCount + 1
        else navigate(directions = directions.drop(1), turn = map.turns.getTurn(next), turnCount = turnCount + 1)
      case None            => navigate(map.directions, turn, turnCount)
    navigate(map.directions, map.turns.getTurn(startAt))

  case class Turn(left: String, right: String) {
    lazy val to: Char => String =
      case 'L'   => left
      case 'R'   => right
      case other => throw IllegalArgumentException(s"'$other' is not a valid direction")
  }

  case class MapOfTheDesert(directions: String, turns: Map[String, Turn])

  extension (turns: Map[String, Turn])
    def getTurn(key: String): Turn =
      turns.getOrElse(key, throw new IllegalArgumentException(s"Position $key not found in map!"))
  end extension

  lazy val parseInput: Seq[String] => MapOfTheDesert = input =>
    val (directions, mapLines) = input.span(!_.isBlank)
    MapOfTheDesert(
      directions = directions.headOption.getOrElse(""),
      turns      = mapLines
        .drop(1)
        .foldLeft(Map.empty[String, Turn]):
          case (directions, current) => current match
              case s"$key = ($leftKey, $rightKey)" => directions.updated(key, Turn(leftKey, rightKey))
              case other                           => throw new IllegalArgumentException(s"Map direction not in expected format: $other")
    )

end DayEight
