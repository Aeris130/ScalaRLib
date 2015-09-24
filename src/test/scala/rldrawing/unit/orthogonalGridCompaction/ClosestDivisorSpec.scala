package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.util.ClosestDivisor

@RunWith(classOf[JUnitRunner])
class ClosestDivisorSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  def fixture = new {
    val finder = new ClosestDivisor()
  }
  describe("ClosestDivisor") {

    it ("should throw an exception if the initial divisor is 0") {
      val f = fixture
      import f._

      Given("an initial divisor of 0")
      val divisor = 0

      When("computing the closest divisor")
      Then("an exception should be thrown")
      intercept[Error] {
        finder.findClosestDivisor(1, divisor)
      }

    }

    it ("should return the initial divisor if it divides without remainder") {
      val f = fixture
      import f._

      Given("an initial divisor of 2")
      val divisor = 2

      When("computing a divisor using a number that is evenly divisible by 2")
      val closest = finder.findClosestDivisor(4, divisor)

      Then("the result should be 2")
      closest should be (divisor)

    }

    it ("should find a result that lies above the initial divisor") {

      val f = fixture
      import f._

      Given("an initial divisor of 2 and a value that is evenly divisible by 3")
      val divisor = 2
      val value = 9

      When("computing a divisor")
      val closest = finder.findClosestDivisor(value, divisor)

      Then("the result should be 3")
      closest should be (3)

    }

    it ("should find a result that lies below the initial divisor") {

      val f = fixture
      import f._

      Given("an initial divisor of 4 and a value that is evenly divisible by 3")
      val divisor = 4
      val value = 9

      When("computing a divisor")
      val closest = finder.findClosestDivisor(value, divisor)

      Then("the result should be 3")
      closest should be (3)

    }

  }
}
