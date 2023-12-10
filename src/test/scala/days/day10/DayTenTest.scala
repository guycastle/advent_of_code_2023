package days.day10

import days.BaseTest
import days.day10.DayTen.TileType

class DayTenTest extends BaseTest:

  lazy val test1: Seq[String] = Seq(
    "-L|F7",
    "7S-7|",
    "L|7||",
    "-L-J|",
    "L|-JF"
  )

  lazy val test2: Seq[String] = Seq(
    "7-F7-",
    ".FJ|7",
    "SJLL7",
    "|F--J",
    "LJ.LJ"
  )

  "Day ten's solution" must:

    "correctly parse the maze and find the animal" in:
      val (animalTile1, maze1) = DayTen.parseInput(test1)
      val (animalTile2, maze2) = DayTen.parseInput(test2)
      animalTile1.coordinates mustBe (1, 1)
      animalTile2.coordinates mustBe (0, 2)
      maze1.get(animalTile1.coordinates).value.tileType mustBe TileType.Animal
      maze2.get(animalTile2.coordinates).value.tileType mustBe TileType.Animal

    "correctly calculate the number of steps required to reach halfway across the pipe loop" in:
      DayTen.partOne(test1) mustBe 4
      DayTen.partOne(test2) mustBe 8

end DayTenTest
