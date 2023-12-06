package utils

import scala.io.Source
import scala.util.{Try, Using}

object InputReader:
  lazy val readLines: String => Try[Seq[String]] =
    path => Using(Source.fromResource(path))(source => source.getLines().toSeq)
end InputReader
