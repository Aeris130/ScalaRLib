package rldungeon.unit.strategy

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.cyndeline.scalarlib.rldungeon.dgs._
import rldungeon.help.{GraphLevel, RoomVertex, CorridorEdge}
import scalax.collection.immutable.Graph
import org.scalamock.scalatest.MockFactory

@RunWith(classOf[JUnitRunner])
class ParameterSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  def fixture = new {
    val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
    val estimator = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
  }

  describe("Parameter") {

    it ("should throw an exception if max target value is less than min target value") {

      Given("a max target value and a min target value that is lower than max")
      val max = 1
      val min = 2

      When("constructing a parameter")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        makeParameter(min, max)
      }

    }

    it ("should set the target value as the mean of the max and min value when both values are positive") {

      Given("a max target value and a min target value that are both positive")
      val max = 4
      val min = 1

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be 2.5")
      p.target should be (2.5)

    }

    it ("should set the target value as the mean of the max and min value when both values are negative") {

      Given("a max target value and a min target value that are both positive")
      val max = -1
      val min = -4

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be -2.5")
      p.target should be (-2.5)

    }

    it ("should set the target value as the mean of the max and min value when min is negative and max is positive") {

      Given("a max target value and a min target value that are both positive")
      val max = 1
      val min = -1

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be 0")
      p.target should be (0)

    }

    it ("should set the target value as 0 if both min and max is zero") {

      Given("a max target value and a min target value that are both 0")
      val max = 0
      val min = 0

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be 0")
      p.target should be (0)

    }

    it ("should set the target value as x if both min and max is x") {

      Given("a max target value and a min target value that are both 5")
      val max = 5
      val min = 5

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be 5")
      p.target should be (5)

    }

    it ("should set the target value as -x if both min and max is -x") {

      Given("a max target value and a min target value that are both -5")
      val max = -5
      val min = -5

      When("constructing a parameter")
      val p = makeParameter(min, max)

      Then("the target should be -5")
      p.target should be (-5)

    }

    it ("should return the value specified by its estimator object when computing the value of a graph") {

      Given("an input graph")
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val estimator = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
      val p = makeParameter(0, 0, estimator)

      When("estimating a value for the graph")
      Then("the estimator should receive the graph")
      val returnedValue = 3
      (estimator.value _) expects(g) returns(returnedValue) once()

      // Start test
      val value = p.estimate(g)

      And("the result should be the value returned by the estimator")
      value should be (returnedValue)

    }

    it ("should validate a graph whose estimate is within the target bounds") {

      Given("an input graph and an estimator that values the graph as 2")
      val f = fixture
      import f._
      (estimator.value _) expects(g) returns(2) once()

      When("validating the graph using a parameter with min value 1 and max value 3")
      val p = makeParameter(1, 3, estimator)
      val isValid = p.validate(g)

      Then("the result should be true")
      isValid should be (true)

    }

    it ("should validate a graph whose estimate is on the edge of the target bounds") {

      Given("an input graph and an estimator that values the graph as 2")
      val f = fixture
      import f._
      (estimator.value _) expects(g) returns(2) once()

      When("validating the graph using a parameter with min value 2 and max value 2")
      val p = makeParameter(2, 2, estimator)
      val isValid = p.validate(g)

      Then("the result should be true")
      isValid should be (true)

    }

    it ("should reject a graph whose estimate is outside the target bounds") {

      Given("an input graph and an estimator that values the graph as 2")
      val f = fixture
      import f._
      (estimator.value _) expects(g) returns(2) once()

      When("validating the graph using a parameter with min value higher than 2")
      val p = makeParameter(4, 5, estimator)
      val isValid = p.validate(g)

      Then("the result should be false")
      isValid should be (false)

    }

    /*
     *
     *  Testing compairsons
     *
     *
     */

    it ("should be indifferent to two equal estimates") {

      Given("a parameter with target 4")
      val parameter = makeParameter(2, 6)

      When("comparing an old estimate 1 with a new estimate 1")
      val result = parameter.compare(1.0, 1.0)

      Then("the new estimate should be indifferent")
      result should be (Indifferent)

    }

    it ("should use the specified precision when checking if two estimates are equal") {

      Given("a parameter with precision 0.001")
      val parameter = makeParameter(0, 0, null, 0.001)

      When("comparing an old estimate 0.0 with a new estimate 0.01, and an old estimate 0.0 with a new estimate 0.00001")
      val result1 = parameter.compare(0.0, 0.01)
      val result2 = parameter.compare(0.0, 0.00001)

      Then("the first estimate should be rejected")
      result1 should be (Rejected)

      And("the second estimate should be indifferent (i.e equal)")
      result2 should be (Indifferent)

    }

    it ("should favor an estimate that is closer to the target when the target is positive") {

      Given("a parameter with target 4")
      val parameter = makeParameter(2, 6)

      When("comparing an old estimate 0 with a new estimate 1")
      val result = parameter.compare(0, 1)

      Then("the new estimate should be accepted")
      result should be (Accepted)

    }

    it ("should reject an estimate that is further away from the target when the target is positive") {

      Given("a parameter with target 4")
      val parameter = makeParameter(2, 6)

      When("comparing an old estimate 5 with a new estimate 6")
      val result = parameter.compare(5, 6)

      Then("the new estimate should be rejected")
      result should be (Rejected)

    }

    it ("should favor an estimate that is closer to the target when the target is negative") {

      Given("a parameter with target -4")
      val parameter = makeParameter(-6, -2)

      When("comparing an old estimate 2 with a new estimate 1")
      val result = parameter.compare(2, 1)

      Then("the new estimate should be accepted")
      result should be (Accepted)

    }

    it ("should reject an estimate that is further away from the target when the target is negative") {

      Given("a parameter with target -4")
      val parameter = makeParameter(-6, -2)

      When("comparing an old estimate 5 with a new estimate 6")
      val result = parameter.compare(5, 6)

      Then("the new estimate should be rejected")
      result should be (Rejected)

    }

    // Only testing acceptence with mixed negatives and positives

    it ("should favor an estimate that is closer to the target when the target is positive and the estimates are negative") {

      Given("a parameter with target 4")
      val parameter = makeParameter(2, 6)

      When("comparing an old estimate -5 with a new estimate -4")
      val result = parameter.compare(-5, -4)

      Then("the new estimate should be accepted")
      result should be (Accepted)

    }

    it ("should favor an estimate that is closer to the target when the target is negative and the estimates negative") {

      Given("a parameter with target -4")
      val parameter = makeParameter(-6, -2)

      When("comparing an old estimate -1 with a new estimate -2")
      val result = parameter.compare(-1, -2)

      Then("the new estimate should be accepted")
      result should be (Accepted)

    }


  }

  private def makeParameter(min: Double, max: Double, estimator: ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge] = null, precision: Double = 0.0001, prio: Boolean = false) =
    new Parameter[GraphLevel, RoomVertex, CorridorEdge]("Param", min, max, precision, estimator, prio)
}
