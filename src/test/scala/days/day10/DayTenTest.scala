package days.day10

import days.BaseTest

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

  lazy val test3: Seq[String] = Seq(
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  )

  lazy val test4: Seq[String] = Seq(
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  )

  lazy val test5: Seq[String] = Seq(
    "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L"
  )

  "Day ten's solution" must:

    "correctly calculate the number of steps required to reach halfway across the pipe loop" in:
      DayTen.partOne(test1) mustBe 4
      DayTen.partOne(test2) mustBe 8

    "correctly calculate the space enclosed in the loop" in:
      DayTen.partTwo(test3) mustBe 4
      DayTen.partTwo(test4) mustBe 8
      DayTen.partTwo(test5) mustBe 10

end DayTenTest
