package days.day05

import days.DailyChallenge

import scala.util.matching.Regex

object DayFive extends DailyChallenge[Long]:

  override lazy val day: Int = 5

  override def partOne(input: Seq[String]): Long =
    (for
      almanac: Almanac <- almanacParser(input)
      closest <- almanac.seeds.map(almanac.locationForSeed).minOption
    yield closest).getOrElse(0)

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  private lazy val numberRegex: Regex = "[0-9]+".r

  private lazy val stringToLongs: String => Seq[Long] = str => numberRegex.findAllIn(str).map(_.toLong).toSeq

  case class Almanac(seeds: Seq[Long],
                     seedsToSoils: Seq[RangeMap],
                     soilsToFertilizers: Seq[RangeMap],
                     fertilizersToWaters: Seq[RangeMap],
                     watersToLights: Seq[RangeMap],
                     lightsToTemperatures: Seq[RangeMap],
                     temperaturesToHumidity: Seq[RangeMap],
                     humidityToLocations: Seq[RangeMap]):
    def locationForSeed(seed: Long): Long =
      val soil = this.seedsToSoils.get(seed)
      val fertilizer = this.soilsToFertilizers.get(soil)
      val water = this.fertilizersToWaters.get(fertilizer)
      val light = this.watersToLights.get(water)
      val temp = this.lightsToTemperatures.get(light)
      val humidity = this.temperaturesToHumidity.get(temp)
      this.humidityToLocations.get(humidity)
  end Almanac

  case class RangeMap(source: Long, destination: Long, length: Long):
    def contains(key: Long): Boolean = (source until source + length).contains(key)
    // may return negative number if not in range, should always be used in conjunction with contains
    def get(key: Long): Long = destination + (key - source)
  end RangeMap

  extension (ranges: Seq[RangeMap])
    def get(key: Long): Long = ranges.collectFirst {
      case range if range.contains(key) => range.get(key)
    }.getOrElse(key)
  end extension

  private def almanacSectionParser(input: Seq[String], key: String): Seq[RangeMap] = input
    .dropWhile(!_.startsWith(key))
    .tail
    .takeWhile(!_.isBlank)
    .map(stringToLongs)
    .collect {
      case Seq(destinationRangeStart, sourceRangeStart, rangeLength) =>
        RangeMap(source = sourceRangeStart, destination = destinationRangeStart, length = rangeLength)
    }.sortBy(_.source)

  lazy val almanacParser: Seq[String] => Option[Almanac] = input =>
    input.collectFirst { case s"seeds: $numbers" => stringToLongs(numbers) }
      .map(seeds => Almanac(
        seeds = seeds,
        seedsToSoils = almanacSectionParser(input = input, key = "seed-to-soil"),
        soilsToFertilizers = almanacSectionParser(input = input, key = "soil-to-fertilizer"),
        fertilizersToWaters = almanacSectionParser(input = input, key = "fertilizer-to-water"),
        watersToLights = almanacSectionParser(input = input, key = "water-to-light"),
        lightsToTemperatures = almanacSectionParser(input = input, key = "light-to-temperature"),
        temperaturesToHumidity = almanacSectionParser(input = input, key = "temperature-to-humidity"),
        humidityToLocations = almanacSectionParser(input = input, key = "humidity-to-location")
      ))

end DayFive
