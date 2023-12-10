package days.day10

import days.DailyChallenge
import days.day10.DayTen.Direction.West
import days.day10.DayTen.TileType.Animal

import scala.annotation.{tailrec, targetName}

object DayTen extends DailyChallenge[Int]:

  override lazy val day: Int = 10

  override def partOne(input: Seq[String]): Int =
    val (animal, maze) = parseInput(input)
    mazeHalfwayPoint(animalTile = animal, maze = maze)

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  def mazeHalfwayPoint(animalTile: Tile, maze: Maze): Int =
    @tailrec
    def go(tile: Tile, origin: Option[Coordinates] = None, steps: Int = 0): Int = origin match
      case Some(o) => (for
          newDirection <- tile.tileType.ports.find(dir => dir.goFrom(tile.coordinates) != o)
          nextTile     <- maze.get(newDirection.goFrom(tile.coordinates))
        yield nextTile) match
          case Some(next) if next.tileType == Animal => steps + 1
          case Some(next)                            => go(next, Some(tile.coordinates), steps = steps + 1)
          case _                                     => throw new IllegalArgumentException(s"Reached dead end at ${tile.coordinates}")
      case None    =>
        if tile.tileType != TileType.Animal then throw new IllegalArgumentException("First tile must be animal!")
        else
          maze.getConnectedPipes(tile.coordinates).headOption match
            case None           => steps
            case Some(nextTile) => go(tile = nextTile, origin = Some(tile.coordinates), steps = steps + 1)
    val loop = go(tile = animalTile)
    if loop % 2 != 0 then (loop / 2) + 1 else loop / 2

  type Row  = Seq[Tile]
  type Maze = Seq[Row]
  extension (maze: Maze)
    def get:               Coordinates => Option[Tile] = (x, y) =>
      if x < 0 || y < 0 then None
      else
        for
          row  <- maze.lift(y)
          tile <- row.lift(x)
        yield tile
    def getConnectedPipes: Coordinates => Seq[Tile]    = coord =>
      maze.get(coord) match
        case Some(tile) if tile.tileType == TileType.Ground => Seq.empty
        case Some(tile) if tile.tileType == TileType.Animal =>
          Direction.values
            .flatMap(dir => maze.get(dir.goFrom(tile.coordinates)))
            .find(_.connections.contains(tile.coordinates))
            .toSeq
        case Some(tile)                                     => tile.tileType.ports.flatMap(dir => maze.get(dir.goFrom(tile.coordinates)))
        case None                                           => Seq.empty
  end extension
  object Maze:
    def empty: Maze = Seq.empty
  end Maze

  type Coordinates = (Int, Int)
  extension (c: Coordinates)
    @targetName("plus")
    def + : Coordinates => Coordinates = (a, b) => (c._1 + a, c._2 + b)
  end extension

  enum Direction(val goFrom: Coordinates => Coordinates):
    case North     extends Direction(goFrom = _ + (0, -1))
    case NorthWest extends Direction(goFrom = _ + (-1, -1))
    case NorthEast extends Direction(goFrom = _ + (1, -1))
    case West      extends Direction(goFrom = _ + (-1, 0))
    case East      extends Direction(goFrom = _ + (1, 0))
    case South     extends Direction(goFrom = _ + (0, 1))
    case SouthWest extends Direction(goFrom = _ + (-1, 1))
    case SouthEast extends Direction(goFrom = _ + (1, 1))
  end Direction

  lazy val parseInput: Seq[String] => (Tile, Maze) = input =>
    val (start, maze) = input.zipWithIndex.foldLeft((Option.empty[Tile], Maze.empty)):
      case ((animal, maze), (line, y)) =>
        val tiles       = line.zipWithIndex.map((char, x) =>
          Tile(
            coordinates = (x, y),
            tileType    = TileType.values
              .find(_.char == char)
              .getOrElse(throw new IllegalArgumentException(s"Invalid tile character: $char"))
          )
        )
        val updatedMaze = maze :+ tiles
        (animal.orElse(tiles.find(_.tileType == Animal)), updatedMaze)
    (start.getOrElse(throw new IllegalArgumentException("No animal found!")), maze)

  case class Tile(coordinates: Coordinates, tileType: TileType):
    def connections: Seq[Coordinates] = this.tileType.ports.map(_ goFrom this.coordinates)

  enum TileType(val char: Char, val ports: Seq[Direction]):
    case Vertical   extends TileType(char = '|', ports = Seq(Direction.North, Direction.South))
    case Horizontal extends TileType(char = '-', ports = Seq(Direction.West, Direction.East))
    case NorthEast  extends TileType(char = 'L', ports = Seq(Direction.North, Direction.East))
    case NorthWest  extends TileType(char = 'J', ports = Seq(Direction.North, West))
    case SouthWest  extends TileType(char = '7', ports = Seq(Direction.West, Direction.South))
    case SouthEast  extends TileType(char = 'F', ports = Seq(Direction.East, Direction.South))
    case Ground     extends TileType(char = '.', ports = Seq.empty)
    case Animal     extends TileType(char = 'S', ports = Seq.empty)
  end TileType
end DayTen
