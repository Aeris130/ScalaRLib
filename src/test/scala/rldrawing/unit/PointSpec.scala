package rldrawing.unit

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class PointSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

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
