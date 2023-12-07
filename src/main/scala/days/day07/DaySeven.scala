package days.day07

import days.DailyChallenge

import scala.math.Ordering.Implicits._

object DaySeven extends DailyChallenge[Int]:

  override lazy val day: Int = 7

  override def partOne(input: Seq[String]): Int =
    input.map(parseInput).sorted.zipWithIndex.map((hand, idx) => hand.bid * (idx + 1)).sum

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  lazy val cardMap: Map[Char, Int] = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
    .reverse
    .zipWithIndex
    .toMap

  enum HandType(val id: Int):
    case FiveOfAKind extends HandType(id = 6)
    case FourOfAKind extends HandType(id = 5)
    case FullHouse extends HandType(id = 4)
    case ThreeOfAKind extends HandType(id = 3)
    case TwoPairs extends HandType(id = 2)
    case OnePair extends HandType(id = 1)
    case HighCard extends HandType(id = 0)
  end HandType
  object HandType:
    // Stronger first, weaker last
    lazy val determineType: Seq[Int] => HandType = counts =>
      if counts contains 5 then FiveOfAKind
      else if counts contains 4 then FourOfAKind
      else if (counts contains 2) && (counts contains 3) then FullHouse
      else if counts contains 3 then ThreeOfAKind
      else if counts.count(_ == 2) == 2 then TwoPairs
      else if counts contains 2 then OnePair
      else if !counts.exists(_ != 1) then HighCard
      else throw new IllegalArgumentException("Could not determine hand type")
  end HandType

  lazy val countCards: Seq[Int] => Seq[Int] = _.foldLeft(Map.empty[Int, Int]):
    case (counts, card) => counts.updatedWith(card)(value => Some(value.getOrElse(0) + 1))
  .values
  .toSeq

  case class Hand(cards: Seq[Int], bid: Int, handType: HandType) extends Ordered[Hand]:
    override def compare(that: Hand): Int = this.handType.id compareTo that.handType.id match
      case 0 => this.cards.indices.collectFirst {
        case idx if (this.cards(idx) compareTo that.cards(idx)) != 0 => this.cards(idx) compareTo that.cards(idx)
      }.getOrElse(0)
      case x => x
  end Hand

  lazy val parseInput: String => Hand = line =>
    val (cards, bid) = line.span(!_.isWhitespace)
    val cardNumbers = cards.flatMap(cardMap.get)
    val cardCounts = countCards(cardNumbers)
    // Stronger first, weaker last
    val handType = HandType.determineType(cardCounts)
    Hand(cards = cardNumbers, bid = bid.strip.toInt, handType)

end DaySeven
