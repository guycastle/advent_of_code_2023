package days.day16

import days.DailyChallenge

object DaySixteen extends DailyChallenge[Int]:
  override lazy val day: Int = 16

  override def partOne(input: Seq[String]): Int =
    countEnergised(startingPoint = Coords(x = 0, y = 0), direction = Direction.Right, contraption = parseInput(input))

  override def partTwo(input: Seq[String]): Int =
    val contraption      = parseInput(input)
    val columnCount      = contraption.head.size
    val rowCount         = contraption.size
    val horizontalStarts = (0 until rowCount).flatMap(y =>
      Seq((Coords(x = 0, y = y), Direction.Right), (Coords(x = columnCount - 1, y = y), Direction.Left))
    )
    val verticalStarts   = (0 until columnCount).flatMap(x =>
      Seq((Coords(x = x, y = 0), Direction.Down), (Coords(x = x, y = rowCount - 1), Direction.Up))
    )
    (horizontalStarts ++ verticalStarts).foldLeft(0):
      case (max, (coords, direction)) =>
        val energised = countEnergised(startingPoint = coords, direction = direction, contraption = contraption)
        if energised > max then energised else max

  @main def run(): Unit = evaluate()

  case class Tile(action: Direction => Seq[Direction], isEnergised: Boolean = false)
  case class Coords(x: Int, y: Int)

  enum Direction(val go: Coords => Coords):
    case Up    extends Direction(go = c => c.copy(y = c.y - 1))
    case Down  extends Direction(go = c => c.copy(y = c.y + 1))
    case Left  extends Direction(go = c => c.copy(x = c.x - 1))
    case Right extends Direction(go = c => c.copy(x = c.x + 1))
  end Direction

  type Contraption = Seq[Seq[Tile]]

  private def countEnergised(startingPoint: Coords, direction: Direction, contraption: Contraption): Int =
    val (_, mapped) = navigate(c = startingPoint, dir = direction, contraption = contraption)
    mapped.map(_.count(_.isEnergised)).sum

  /**
    * Navigate the contraption recursively
    * @param c
    *   the coordinates to evaluate
    * @param dir
    *   the direction we're coming from
    * @param contraption
    *   the contraption
    * @param splits
    *   a list of coordinates where the beam was already split. So we don't go through it twice
    * @return
    *   an updated contraption with updated tiles that were energised
    */
  private def navigate(c:           Coords,
                       dir:         Direction,
                       contraption: Contraption,
                       splits:      Seq[Coords] = Seq.empty): (Seq[Coords], Contraption) = (for
    row  <- contraption.lift(c.y)
    tile <- row.lift(c.x)
  yield (row, tile)) match
    case Some((row, tile)) =>
      val updatedContraption =
        if !tile.isEnergised then contraption.updated(c.y, row.updated(c.x, tile.copy(isEnergised = true)))
        else contraption
      val newDirs            = tile.action(dir)
      if newDirs.size == 1 then
        navigate(c = newDirs.head.go(c), dir = newDirs.head, contraption = updatedContraption, splits = splits)
      else if !splits.contains(c) then
        newDirs.foldLeft((splits :+ c, updatedContraption)):
          case ((splitPoints, currentContraption), newDir) =>
            navigate(c = newDir.go(c), dir = newDir, contraption = currentContraption, splits = splitPoints)
      else (splits, contraption)
    case None              => (splits, contraption)

  lazy val parseInput: Seq[String] => Contraption = _.map:
    _.collect:
      case '.'  => Tile(Seq(_))
      case '-'  => Tile(
          action =
            case Direction.Up | Direction.Down            => Seq(Direction.Left, Direction.Right)
            case dir @ (Direction.Left | Direction.Right) => Seq(dir)
        )
      case '|'  => Tile(
          action =
            case Direction.Left | Direction.Right      => Seq(Direction.Up, Direction.Down)
            case dir @ (Direction.Up | Direction.Down) => Seq(dir)
        )
      case '/'  => Tile(
          action =
            case Direction.Up    => Seq(Direction.Right)
            case Direction.Down  => Seq(Direction.Left)
            case Direction.Left  => Seq(Direction.Down)
            case Direction.Right => Seq(Direction.Up)
        )
      case '\\' => Tile(
          action =
            case Direction.Up    => Seq(Direction.Left)
            case Direction.Down  => Seq(Direction.Right)
            case Direction.Left  => Seq(Direction.Up)
            case Direction.Right => Seq(Direction.Down)
        )
end DaySixteen
