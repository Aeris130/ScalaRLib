package rldrawing.unit

import net.cyndeline.rlcommon.util.Point
import testHelpers.SpecImports

class PointSpec extends SpecImports {

  describe("Point") {

    it ("should throw an exception when using a negative x coordinate") {

      Given("a negative x coordinate")
      val x = -1

      When("creating a new Point")
      Then("an exception should be thrown")
      intercept[Error] {
        Point(x, 1)
      }

    }

    it ("should throw an exception when using a negative y coordinate") {

      Given("a negative y coordinate")
      val y = -1

      When("creating a new Point")
      Then("an exception should be thrown")
      intercept[Error] {
        Point(1, y)
      }

    }

  }
}
