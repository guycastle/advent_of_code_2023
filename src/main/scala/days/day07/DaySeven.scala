package days.day07

import days.DailyChallenge
import days.day07.DaySeven.HandType.*

object DaySeven extends DailyChallenge[Int]:

  override lazy val day: Int = 7

  override def partOne(input: Seq[String]): Int =
    input.map(parseInput(1, _)).sorted.zipWithIndex.map((hand, idx) => hand.bid * (idx + 1)).sum

  override def partTwo(input: Seq[String]): Int =
    input.map(parseInput(2, _)).sorted.zipWithIndex.map((hand, idx) => hand.bid * (idx + 1)).sum

  @main def run(): Unit = evaluate()

  lazy val cardMapPart1: Map[Char, Int] =
    Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse.zipWithIndex.toMap

  lazy val cardMapPart2: Map[Char, Int] =
    Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

  enum HandType(val id: Int):
    case FiveOfAKind  extends HandType(id = 6)
    case FourOfAKind  extends HandType(id = 5)
    case FullHouse    extends HandType(id = 4)
    case ThreeOfAKind extends HandType(id = 3)
    case TwoPairs     extends HandType(id = 2)
    case OnePair      extends HandType(id = 1)
    case HighCard     extends HandType(id = 0)
  end HandType

  lazy val determineTypePartOne: Map[Int, Int] => HandType = map =>
    val counts = map.values.toSeq
    if counts contains 5 then FiveOfAKind
    else if counts contains 4 then FourOfAKind
    else if (counts contains 2) && (counts contains 3) then FullHouse
    else if counts contains 3 then ThreeOfAKind
    else if counts.count(_ == 2) == 2 then TwoPairs
    else if counts contains 2 then OnePair
    else if !counts.exists(_ != 1) then HighCard
    else throw new IllegalArgumentException("Could not determine hand type")

  lazy val determineTypePartTwo: Map[Int, Int] => HandType = counts =>
    val jokers = counts.getOrElse(0, 0)
    determineTypePartOne(counts) match
      case FiveOfAKind  => FiveOfAKind
      case FourOfAKind  => if jokers >= 1 then FiveOfAKind else FourOfAKind
      case FullHouse    => if jokers != 0 then FiveOfAKind else FullHouse
      case ThreeOfAKind => if jokers != 0 then FourOfAKind else ThreeOfAKind
      case TwoPairs     => if jokers == 0 then TwoPairs else if jokers == 1 then FullHouse else FourOfAKind
      case OnePair      => if jokers == 0 then OnePair else ThreeOfAKind
      case HighCard     => if jokers > 0 then OnePair else HighCard

  lazy val countCards: Seq[Int] => Map[Int, Int] = _.foldLeft(Map.empty[Int, Int]):
    case (counts, card) => counts.updatedWith(card)(value => Some(value.getOrElse(0) + 1))

  case class Hand(cards: Seq[Int], bid: Int, handType: HandType) extends Ordered[Hand]:
    override def compare(that: Hand): Int = this.handType.id compareTo that.handType.id match
      case 0 => this.cards.indices
          .collectFirst {
            case idx if (this.cards(idx) compareTo that.cards(idx)) != 0 => this.cards(idx) compareTo that.cards(idx)
          }
          .getOrElse(0)
      case x => x
  end Hand

  def parseInput(part: Int, line: String): Hand =
    val (cardMap, eval) =
      if part == 1 then (cardMapPart1, determineTypePartOne) else (cardMapPart2, determineTypePartTwo)
    val (cards, bid)    = line.span(!_.isWhitespace)
    val cardNumbers     = cards.flatMap(cardMap.get)
    val cardCounts      = countCards(cardNumbers)

    Hand(cards = cardNumbers, bid = bid.strip.toInt, handType = eval(cardCounts))

end DaySeven
