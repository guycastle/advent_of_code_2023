package days.day08

import days.DailyChallenge

import scala.annotation.tailrec

object DayEight extends DailyChallenge[BigInt]:

  override lazy val day: Int = 8

  override def partOne(input: Seq[String]): BigInt =
    stepsToNavigateAcrossDesert(map = parseInput(input), startAt = "AAA", endAt = "ZZZ")

  override def partTwo(input: Seq[String]): BigInt =
    stepsToNavigateAcrossDesert(map = parseInput(input), startAt = "A", endAt = "Z")

  @main def run(): Unit = evaluate()

  private def stepsToNavigateAcrossDesert(map: MapOfTheDesert, startAt: String, endAt: String): BigInt =
    @tailrec
    def navigate(directions: Seq[Char], turn: Turn, turnCount: BigInt = 0): BigInt = directions.headOption match
      case Some(direction) =>
        val next = turn.to(direction)
        if next endsWith endAt then turnCount + 1
        else
          navigate(
            directions = directions.drop(1),
            turn       = map.turns.getTurn(next),
            turnCount  = turnCount + 1
          )
      case None            => navigate(directions = map.directions, turn = turn, turnCount = turnCount)

    lowestCommonMultiplier(map.turns.collect {
      case (key, turn) if key endsWith startAt => navigate(map.directions, turn)
    })

  case class Turn(left: String, right: String):
    lazy val to: Char => String =
      case 'L'   => left
      case 'R'   => right
      case other => throw IllegalArgumentException(s"'$other' is not a valid direction")
  end Turn

  case class MapOfTheDesert(directions: String, turns: Map[String, Turn])

  lazy val lowestCommonMultiplier: Iterable[BigInt] => BigInt = _.reduce(_ lcm _)

  extension (turns: Map[String, Turn])
    private def getTurn(key: String): Turn =
      turns.getOrElse(key, throw new IllegalArgumentException(s"Position $key not found in map!"))
  end extension

  extension (bi:          BigInt)
    // https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor
    private def lcm(that: BigInt): BigInt = (bi * that) / (bi gcd that)
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
