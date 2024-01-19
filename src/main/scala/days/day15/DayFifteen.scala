package days.day15

import days.DailyChallenge

import scala.collection.immutable.VectorMap

object DayFifteen extends DailyChallenge[Int]:

  override lazy val day: Int = 15

  override def partOne(input: Seq[String]): Int = parseInput(input).map(_.aocHash).sum

  override def partTwo(input: Seq[String]): Int = parseInput(input)
    .foldLeft(Map.empty[Int, Box]):
      case (boxes, op) =>
        val step = op.toStep
        boxes.updated(
          step.box,
          step.focalLength match
            case Some(lens) => boxes.getOrElse(step.box, Box(step.box)).updated(step.label, lens)
            case None       => boxes.getOrElse(step.box, Box(step.box)).removed(step.label)
        )
    .values
    .foldLeft(0):
      case (sum, box) => sum + box.focusingPower

  @main def run(): Unit = evaluate()

  case class Box(number: Int, contents: VectorMap[String, Int] = VectorMap.empty):
    def focusingPower: Int = this.contents.values.zipWithIndex.foldLeft(0):
      case (sum, (focalLength, idx)) => sum + ((this.number + 1) * (idx + 1) * focalLength)
    def updated(label: String, lens: Int): Box = this.copy(contents = contents.updated(label, lens))
    def removed(label: String): Box = this.copy(contents = contents.removed(label))
  end Box

  extension (str: String)
    def aocHash: Int  = str.foldLeft(0):
      case (hash, char) if char != '\n' => ((hash + char.toInt) * 17) % 256
      case (hash, _)                    => hash
    def toStep:  Step =
      val (label, op) = str.span:
        case '=' | '-' => false
        case _         => true
      Step(label = label, box = label.aocHash, focalLength = op.drop(1).toIntOption)
  end extension

  case class Step(label: String, box: Int, focalLength: Option[Int])

  private lazy val parseInput: Seq[String] => Seq[String] = lines => lines.flatMap(_.split(','))

end DayFifteen
