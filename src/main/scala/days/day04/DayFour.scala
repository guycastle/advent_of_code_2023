package days.day04

import days.DailyChallenge

import scala.annotation.tailrec
import scala.util.matching.Regex

object DayFour extends DailyChallenge[Int]:

  override lazy val day: Int = 4

  lazy val numberRegex: Regex = "[0-9]+".r

  case class ScratchCardResult(gameId: Int, winningNumbers: Seq[Int], cardNumbers: Seq[Int], cardsWon: Int = 0) {
    val winningNumberMatches: Int = winningNumbers.sorted.intersect(cardNumbers.sorted).size
    lazy val calculatePoints: Int =
      if winningNumberMatches > 0 then (1 * Math.pow(2, winningNumberMatches - 1)).toInt else 0
    lazy val cardsWonTotal:   Int = this.cardsWon + 1
  }

  extension (str: String)
    private def splitBySpaceToNumbers: Seq[Int] = numberRegex.findAllIn(str).map(_.toInt).toSeq

    def toScratchCardResult: Option[ScratchCardResult] = str match
      case s"Card $gameId: $winning | $cardNumbers" => Some(
          ScratchCardResult(
            gameId         = gameId.strip.toInt,
            winningNumbers = winning.splitBySpaceToNumbers,
            cardNumbers    = cardNumbers.splitBySpaceToNumbers
          )
        )
      case other                                    =>
        println(s"$other does not match expected scratch card format")
        None
  end extension

  private lazy val winnings: ScratchCardResult => Range =
    card => (card.gameId + 1) to (card.gameId + card.winningNumberMatches)

  @tailrec
  private def evaluateScratchCards(reversedCards: Seq[ScratchCardResult],
                                   cardMap:       Map[Int, ScratchCardResult] = Map.empty): Seq[ScratchCardResult] =
    reversedCards.headOption match
      case None       => cardMap.values.toSeq
      case Some(card) =>
        val updatedCard =
          if card.winningNumberMatches == 0 then card
          else
            card.copy(cardsWon = winnings(card).map(gameId => cardMap.get(gameId).map(_.cardsWonTotal).getOrElse(0)).sum)
        evaluateScratchCards(reversedCards.drop(1), cardMap.updated(key = updatedCard.gameId, updatedCard))

  override def partOne(input: Seq[String]): Int = input.flatMap(_.toScratchCardResult.map(_.calculatePoints)).sum

  override def partTwo(input: Seq[String]): Int =
    evaluateScratchCards(input.flatMap(_.toScratchCardResult).reverse).foldLeft(0) { (sum, card) =>
      sum + card.cardsWonTotal
    }

  @main def run(): Unit = evaluate()
end DayFour
