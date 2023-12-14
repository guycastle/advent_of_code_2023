package days.day10

import days.DailyChallenge

import scala.annotation.tailrec

object DayTen extends DailyChallenge[Int]:

  override lazy val day: Int = 10

  override def partOne(input: Seq[String]): Int =
    val terrain    = parseInput(input)
    val loopLength = terrain.map(_.count(_.loop)).sum
    if loopLength % 2 == 0 then loopLength / 2 else (loopLength / 2)

  override def partTwo(input: Seq[String]): Int =
    val terrain = parseInput(input)
    terrain.foldLeft(0):
      case (enclosed, row) => countEnclosed(row, enclosed)
  end partTwo

  @main def run(): Unit = evaluate()

  enum Direction:
    case Up, Down, Left, Right
  end Direction
  extension (d: Direction)
    def opposite: Direction                  = d match
      case Direction.Up    => Direction.Down
      case Direction.Down  => Direction.Up
      case Direction.Right => Direction.Left
      case Direction.Left  => Direction.Right
    def move:     Coordinates => Coordinates = c =>
      d match
        case Direction.Up    => c.copy(y = c.y - 1)
        case Direction.Down  => c.copy(y = c.y + 1)
        case Direction.Left  => c.copy(x = c.x - 1)
        case Direction.Right => c.copy(x = c.x + 1)
  end extension

  case class Coordinates(x: Int, y: Int)

  case class Tile(coords: Coordinates, tileType: TileType, loop: Boolean = false)

  type Row     = Seq[Tile]
  type Terrain = Seq[Row]
  extension (t: Terrain)
    def get(coords: Coordinates): Option[Tile] = t.lift(coords.y).flatMap(_.lift(coords.x))
    def update(tile: Tile): Terrain = (for
      row <- t.lift(tile.coords.y)
      _   <- row.lift(tile.coords.x)
    yield t.updated(tile.coords.y, row.updated(tile.coords.x, tile))).getOrElse(t)
    // only works on a freshly parsed input with one tile marked as part of loop
    def start:              Tile    = t
      .flatMap(_.find(_.loop == true))
      .headOption
      .map(start => {
        val startPorts = Seq(
          t.get(Direction.Up.move(start.coords)).filter(_.tileType.ports.contains(Direction.Down)).map(_ => Direction.Up),
          t.get(Direction.Down.move(start.coords))
            .filter(_.tileType.ports.contains(Direction.Up))
            .map(_ => Direction.Down),
          t.get(Direction.Right.move(start.coords))
            .filter(_.tileType.ports.contains(Direction.Left))
            .map(_ => Direction.Right),
          t.get(Direction.Left.move(start.coords))
            .filter(_.tileType.ports.contains(Direction.Right))
            .map(_ => Direction.Left)
        ).flatten.sortBy(_.ordinal)
        start.copy(
          tileType = TileType.values
            .find(_.ports.sortBy(_.ordinal) == startPorts)
            .getOrElse(
              throw new IllegalArgumentException(
                s"Could not determine start tile (${start.coords.x}, ${start.coords.y}) type!"
              )
            )
        )
      })
      .getOrElse(throw new IllegalArgumentException("No starting point found!"))
  end extension

  lazy val parseInput: Seq[String] => Terrain = input =>
    val terrain = input.zipWithIndex.foldLeft(Seq.empty[Seq[Tile]]):
      case (map, (line, y)) => map :+ line.zipWithIndex.map: (char, x) =>
             val tileType = TileType.forChar(char)
             Tile(coords = Coordinates(x = x, y = y), loop = tileType == TileType.Animal, tileType = tileType)
    mapLoop(terrain)
  end parseInput

  @tailrec
  private def mapLoop(terrain: Terrain, tile: Option[Tile] = None, origin: Option[Direction] = None): Terrain =
    (tile, origin) match
      case (Some(current), _) if current.loop => terrain // we're back to our starting point
      case (Some(current), Some(from))        =>
        val direction = current.tileType.ports
          .filterNot(_ == from)
          .headOption
          .getOrElse(throw new IllegalArgumentException("Got nowhere to go!"))
        mapLoop(
          terrain = terrain.update(current.copy(loop = true)),
          tile    = terrain.get(direction.move(current.coords)),
          origin  = Some(direction.opposite)
        )
      case _                                  =>
        val startingPoint = terrain.start
        val direction     =
          startingPoint.tileType.ports.headOption.getOrElse(throw new IllegalArgumentException("Got nowhere to go!"))
        mapLoop(
          terrain = terrain.update(startingPoint),
          tile    = terrain.get(direction.move(startingPoint.coords)),
          origin  = Some(direction.opposite)
        )
  end mapLoop

  @tailrec
  private def countEnclosed(row: Row, enclosed: Int, processed: Row = Seq.empty): Int =
    if row.size <= 2 || !row.exists(_.loop) then enclosed
    else
      lazy val isSouthBoundLoop: Tile => Boolean = tile => tile.loop && TileType.southBound.contains(tile.tileType)
      val (skip, toCheck)         = row.span(!isSouthBoundLoop(_))
      val updatedProcessed        = (processed ++ skip) ++ toCheck.headOption
      val (enclosedPart, remnant) = toCheck.drop(1).span(!isSouthBoundLoop(_))
      val enclosedCount           = if (updatedProcessed.count(isSouthBoundLoop) % 2 != 0) enclosedPart.count(!_.loop) else 0
      countEnclosed(remnant, enclosedCount + enclosed, updatedProcessed)
  end countEnclosed

  enum TileType(val char: Char, val ports: Seq[Direction]):
    case Vertical   extends TileType(char = '|', ports = Seq(Direction.Up, Direction.Down))
    case Horizontal extends TileType(char = '-', ports = Seq(Direction.Left, Direction.Right))
    case UpRight    extends TileType(char = 'L', ports = Seq(Direction.Up, Direction.Right))
    case UpLeft     extends TileType(char = 'J', ports = Seq(Direction.Up, Direction.Left))
    case DownLeft   extends TileType(char = '7', ports = Seq(Direction.Down, Direction.Left))
    case DownRight  extends TileType(char = 'F', ports = Seq(Direction.Down, Direction.Right))
    case Ground     extends TileType(char = '.', ports = Seq.empty)
    case Animal     extends TileType(char = 'S', ports = Seq.empty)
  end TileType
  object TileType:
    def forChar(char: Char): TileType      = TileType.values
      .find(_.char == char)
      .getOrElse(throw new IllegalArgumentException(s"Invalid tile character '$char'"))
    lazy val southBound:     Seq[TileType] = Seq(Vertical, DownLeft, DownRight)
    lazy val northBound:     Seq[TileType] = Seq(Vertical, UpRight, UpLeft)
  end TileType
end DayTen
