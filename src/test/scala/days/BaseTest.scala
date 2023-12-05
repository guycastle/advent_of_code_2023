package days

import org.scalatest.{OptionValues, TryValues}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BaseTest extends AnyWordSpec with Matchers with TryValues with OptionValues
