package days.day14

import days.DailyChallenge
import days.day14.DayFourteen.Direction.Down

object DayFourteen extends DailyChallenge[Int]:
  override lazy val day: Int = 14

  override def partOne(input: Seq[String]): Int =
    moveRoundedRocksUp(parseInput(input)).reverse.zipWithIndex.foldLeft(0):
      case (total, (row, i)) => total + row.values.count(_.rock == Rock.Round) * (i + 1)

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  case class Coords(x: Int, y: Int):
    lazy val go: Direction => Coords =
      case Direction.Up => this.copy(y = y - 1)
      case Direction.Down => this.copy(y = y + 1)
      case Direction.Left => this.copy(x = x - 1)
      case Direction.Right => this.copy(x = x + 1)

  case class Tile(coords: Coords, rock: Rock)

  enum Rock:
   case Round, Cube
  end Rock

  type Row = Map[Int, Tile]
  type DishArray = Seq[Row]
  extension (array: DishArray)
    def remove(tile: Tile): DishArray = array.updated(
      tile.coords.y,
      array.lift(tile.coords.y)
        .getOrElse(Map.empty)
        .removed(tile.coords.x)
    )
    def insert(tile: Tile): DishArray = array.updated(
      tile.coords.y, 
      array.lift(tile.coords.y)
        .getOrElse(Map.empty)
        .updated(tile.coords.x, tile)
    )
    def move(tile: Tile, coords: Coords): DishArray =
      if tile.coords == coords then array else array.remove(tile).insert(tile.copy(coords = coords))
  end extension

  lazy val parseInput: Seq[String] => DishArray = lines =>
    lines.zipWithIndex.map:
      (line, y) => line.zipWithIndex.foldLeft(Map.empty[Int, Tile]):
        case (row, ('O', x)) => row.updated(x, Tile(coords = Coords(x = x, y = y), rock = Rock.Round))
        case (row, ('#', x)) => row.updated(x, Tile(coords = Coords(x = x, y = y), rock = Rock.Cube))
        case (row, _) => row

  enum Direction:
    case Up, Down, Left, Right
  end Direction

  def moveRoundedRocksUp(array: DishArray): DishArray =
    array.drop(1).foldLeft(array):
      case (updatedArray, row) =>
        row.foldLeft(updatedArray):
          case (dish, (_, tile)) if tile.rock == Rock.Round =>
            dish.slice(0, tile.coords.y).flatMap(_.get(tile.coords.x)).lastOption match
              case Some(block) if block.coords.y + 1 != tile.coords.y => dish.move(tile = tile, coords = block.coords.go(Direction.Down))
              case None if tile.coords.y != 0 => dish.move(tile = tile, coords = tile.coords.copy(y = 0))
              case _ => dish
          case (dish, _) => dish

end DayFourteen
