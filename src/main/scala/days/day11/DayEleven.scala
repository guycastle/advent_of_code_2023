package days.day11

import days.DailyChallenge
import days.day11.DayEleven.Space.Empty
import scala.math.abs

object DayEleven extends DailyChallenge[Int]:
  override lazy val day: Int = 11

  override def partOne(input: Seq[String]): Int =
    parseInput(input).map(distanceBetweenGalaxies).sum

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  case class Galaxy(x: Int, y: Int)

  type Row = Seq[Space]
  type Universe = Seq[Row]
  type GalaxyPair = (Galaxy, Galaxy)

  enum Space:
    case Galaxy, Empty
  end Space

  lazy val distanceBetweenGalaxies: GalaxyPair => Int =
    case (Galaxy(x1, y1), Galaxy(x2, y2)) => abs(x1 - x2) + abs(y1 - y2)


  lazy val parseInput: Seq[String] =>  Seq[GalaxyPair] = input =>
    val unexpanded = input.map(_.map:
      case '#' => Space.Galaxy
      case _ => Space.Empty
    )
    val galaxies = unexpanded.headOption match
      case Some(row) =>
        val verticalEmptySpaceIndices = row.indices.filter(i => !unexpanded.exists(_.lift(i).contains(Space.Galaxy)))
        val expanded = unexpanded.flatMap:
          row =>
            val expandedRow = verticalEmptySpaceIndices.foldRight(row):
              case (idx, row) =>
                val (before, after) = row.splitAt(idx)
                (before :+ Empty) ++ after
            if row.forall(_ == Space.Empty) then Seq(expandedRow, expandedRow) else Seq(expandedRow)
        expanded.zipWithIndex.flatMap:
          case (row, y) => row.zipWithIndex.collect:
            case (space, x) if space == Space.Galaxy => Galaxy(x, y)

      case None => throw new IllegalArgumentException("Empty universe")

    def pair(galaxies: Seq[Galaxy], pairs: Seq[GalaxyPair] = Seq.empty): Seq[GalaxyPair] =
      galaxies.headOption match
        case Some(galaxy) if galaxies.size >= 2 =>
          val newPairs = galaxies.drop(1).map:
              other => (galaxy, other)
          pair(galaxies = galaxies.drop(1), pairs = pairs ++ newPairs)
        case _ => pairs

    pair(galaxies)


end DayEleven
