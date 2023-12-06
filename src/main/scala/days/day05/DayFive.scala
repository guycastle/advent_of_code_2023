package days.day05

import days.DailyChallenge

import java.text.NumberFormat
import java.util.Locale
import java.util.concurrent.atomic.AtomicLong
import scala.collection.immutable.NumericRange
import scala.util.matching.Regex

object DayFive extends DailyChallenge[Long]:

  override lazy val day: Int = 5

  override def partOne(input: Seq[String]): Long = closestLocation(input = input, seedParser = seedListParser)

  override def partTwo(input: Seq[String]): Long = closestLocation(input = input, seedParser = seedRangeParser)

  lazy val seedListParser: String => (Seq[SeedType], Long) = str =>
    val seeds = stringToLongs(str)
    (seeds, seeds.size)

  lazy val seedRangeParser: String => (Seq[SeedType], Long) = str =>
    val ranges = stringToLongs(str)
      .sliding(2, 2)
      .toSeq
      .map { case Seq(start, length) => (start until (start + length)) }
      .sortBy(_.start)
      .foldLeft(Seq.empty[NumericRange[Long]]) {
        case (ranges, range) => ranges.lastOption match
            case Some(previous) if previous.end >= range.start =>
              ranges.updated(ranges.size - 1, (previous.start until range.end))
            case _                                             => ranges :+ range
      }
    (ranges, ranges.map(_.length).sum)

  def almanacParser(input: Seq[String], seedParser: String => (Seq[SeedType], Long)): Option[Almanac] = input
    .collectFirst { case s"seeds: $numbers" => seedParser(numbers) }
    .map((seeds, total) => {
      val seedMap     = almanacSectionParser(input = input, key = "seed-to-soil")
      val locationMap = almanacSectionParser(input = input, key = "humidity-to-location")
      Almanac(
        seeds                  = seeds,
        totalSeeds             = total,
        seedsToSoils           = seedMap,
        soilsToFertilizers     = almanacSectionParser(input = input, key = "soil-to-fertilizer"),
        fertilizersToWaters    = almanacSectionParser(input = input, key = "fertilizer-to-water"),
        watersToLights         = almanacSectionParser(input = input, key = "water-to-light"),
        lightsToTemperatures   = almanacSectionParser(input = input, key = "light-to-temperature"),
        temperaturesToHumidity = almanacSectionParser(input = input, key = "temperature-to-humidity"),
        humidityToLocations    = locationMap
      )
    })

  @main def run(): Unit = evaluate()

  private lazy val pctFmt: NumberFormat = NumberFormat.getPercentInstance(Locale.UK)

  private def closestLocation(input: Seq[String], seedParser: String => (Seq[SeedType], Long)): Long =
    val counter = new AtomicLong(0)
    (for
      almanac <- almanacParser(input, seedParser)
      closest <- almanac.seeds.foldLeft(Option.empty[Long]) {
                   case (closest, seed: Long)                =>
                     val seedLocation = almanac.locationForSeed(seed)
                     if closest.forall(_ > seedLocation) then Some(seedLocation) else closest
                   case (closest, range: NumericRange[Long]) => range.foldLeft(closest) {
                       case (rangeClosest, seed) =>
                         println(
                           s"Evaluating seed $seed (${pctFmt.format(counter.incrementAndGet.toDouble / almanac.totalSeeds)})"
                         )
                         val seedLocation = almanac.locationForSeed(seed)
                         if rangeClosest.forall(_ > seedLocation) then Some(seedLocation) else rangeClosest
                     }
                 }
    yield closest).getOrElse(-1)

  private lazy val numberRegex: Regex = "[0-9]+".r

  type SeedType = Long | NumericRange[Long]

  case class Almanac(seeds:                  Seq[SeedType],
                     totalSeeds:             Long,
                     seedsToSoils:           Seq[RangeMap],
                     soilsToFertilizers:     Seq[RangeMap],
                     fertilizersToWaters:    Seq[RangeMap],
                     watersToLights:         Seq[RangeMap],
                     lightsToTemperatures:   Seq[RangeMap],
                     temperaturesToHumidity: Seq[RangeMap],
                     humidityToLocations:    Seq[RangeMap]):
    def locationForSeed(seed: Long): Long = this.humidityToLocations.get(
      this.temperaturesToHumidity.get(
        this.lightsToTemperatures.get(
          this.watersToLights.get(this.fertilizersToWaters.get(this.soilsToFertilizers.get(this.seedsToSoils.get(seed))))
        )
      )
    )
  end Almanac

  case class RangeMap(source: Long, destination: Long, length: Long):
    def contains(key: Long): Boolean = (source until (source + length)).contains(key)
    // may return negative number if not in range, should always be used in conjunction with contains
    def get(key:      Long): Long    = destination + (key - source)
  end RangeMap

  extension (ranges: Seq[RangeMap])
    def get(key: Long): Long = ranges
      .collectFirst {
        case range if range.contains(key) => range.get(key)
      }
      .getOrElse(key)
  end extension

  private def almanacSectionParser(input: Seq[String], key: String): Seq[RangeMap] = input
    .dropWhile(!_.startsWith(key))
    .tail
    .takeWhile(!_.isBlank)
    .map(stringToLongs)
    .collect {
      case Seq(destinationRangeStart, sourceRangeStart, rangeLength) =>
        RangeMap(source = sourceRangeStart, destination = destinationRangeStart, length = rangeLength)
    }
    .sortBy(_.source)

  lazy val stringToLongs: String => Seq[Long] = str => numberRegex.findAllIn(str).map(_.toLong).toSeq

end DayFive
