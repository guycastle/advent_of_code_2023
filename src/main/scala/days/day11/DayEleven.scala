package days.day11

import days.DailyChallenge

import scala.annotation.tailrec
import scala.math.abs

object DayEleven extends DailyChallenge[Long]:
  override lazy val day: Int = 11

  override def partOne(input: Seq[String]): Long =
    parseInput(input = input, expandBy = 1).map(distanceBetweenGalaxies).sum

  override def partTwo(input: Seq[String]): Long =
    parseInput(input = input, expandBy = 999999).map(distanceBetweenGalaxies).sum

  @main def run(): Unit = evaluate()

  case class Galaxy(x: Long, y: Long)

  type Row        = Seq[Galaxy]
  type Universe   = Seq[Row]
  type GalaxyPair = (Galaxy, Galaxy)

  lazy val distanceBetweenGalaxies: GalaxyPair => Long =
    case (Galaxy(x1, y1), Galaxy(x2, y2)) => abs(x1 - x2) + abs(y1 - y2)

  def parseInput(input: Seq[String], expandBy: Int): Seq[GalaxyPair] =
    val universe: Universe = input.headOption match
      case Some(row) =>
        val verticalEmptySpaceIndices = row.indices.filter(i => !input.exists(_.lift(i).contains('#')))
        val (expanded, _)             = input.zipWithIndex.foldLeft(Seq.empty[Row], 0L):
          case ((rows, yOffset), (row, y)) =>
            val newYOffset               = if row.forall(_ == '.') then yOffset + expandBy else yOffset
            val (expandedRowGalaxies, _) = row.zipWithIndex.foldLeft(Seq.empty[Galaxy], 0L):
              case ((galaxies, xOffset), (space, x)) =>
                if verticalEmptySpaceIndices contains x then (galaxies, xOffset + expandBy)
                else
                  space match
                    case '#' => (galaxies :+ Galaxy(x = x + xOffset, y = y + newYOffset), xOffset)
                    case '.' => (galaxies, xOffset)
            (rows :+ expandedRowGalaxies, newYOffset)
        expanded
      case None      => throw new IllegalArgumentException("Empty universe")
      
    universe.flatten.combinations(2)

end DayEleven
