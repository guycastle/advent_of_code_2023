package days

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{OptionValues, TryValues}

trait BaseTest extends AnyWordSpec with Matchers with TryValues with OptionValues
