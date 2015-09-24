package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.ParameterAcceptRatio
import testHelpers.SpecImports

class ParameterAcceptRatioSpec extends SpecImports {

  def fixture = new {
    val p1 = makeParameter
    val p2 = makeParameter
    val p3 = makeParameter
    val p4 = makeParameter
  }

  describe("ParameterAcceptRatio") {

    it ("should throw an exception if the accept ratio is <= 0") {

      Given("two accept ratios -1 and 0")
      val r1 = -1
      val r2 = 0

      When("instantiating the ParameterAcceptRatio")
      Then("an Exception should be thrown")
      intercept[IllegalArgumentException] {
        new ParameterAcceptRatio(r1)
      }
      intercept[IllegalArgumentException] {
        new ParameterAcceptRatio(r2)
      }

    }

    it ("should reject a set of accepting parameters that doesn't exceed the rejecting set by the specified ratio") {

      Given("an accept ratio of 0.2")
      val f = fixture
      import f._
      val validator = new ParameterAcceptRatio(0.2)

      When("validating a set of 1 accepting parameter and 3 rejecting")
      val result = validator.levelModificationValidates(Set(p1), Set(p2, p3, p4))

      Then("the result should be false")
      result should be (false)

    }

    it ("should accept a set of accepting parameters that equals the rejecting set by the specified ratio") {

      Given("an accept ratio of 100%")
      val f = fixture
      import f._
      val validator = new ParameterAcceptRatio(1)

      When("validating a set of 2 accepting parameters and 1 rejecting")
      val result = validator.levelModificationValidates(Set(p1, p2), Set(p3))

      Then("the result should be true")
      result should be (true)

    }

    it ("should accept a set of accepting parameters that exceeds the rejecting set by the specified ratio") {

      Given("an accept ratio of 100%")
      val f = fixture
      import f._
      val validator = new ParameterAcceptRatio(1)

      When("validating a set of 3 accepting parameters and 1 rejecting")
      val result = validator.levelModificationValidates(Set(p1, p2, p4), Set(p3))

      Then("the result should be true")
      result should be (true)

    }

    it ("should reject two empty parameter sets") {

      Given("a low accept ratio")
      val validator = new ParameterAcceptRatio(0.001)

      When("no parameters accept or reject a change")
      val result = validator.levelModificationValidates(Set(), Set())

      Then("the result should be false")
      result should be (false)

    }

    it ("should accept when the rejecting parameter set is empty") {

      Given("a high accept ratio")
      val f = fixture
      import f._
      val validator = new ParameterAcceptRatio(2)

      When("no parameter rejects")
      val result = validator.levelModificationValidates(Set(p1), Set())

      Then("the result should be true")
      result should be (true)

    }

    it ("should reject when the accepting parameter set is empty") {

      Given("a low accept ratio")
      val f = fixture
      import f._
      val validator = new ParameterAcceptRatio(0.001)

      When("no parameters accept a change")
      val result = validator.levelModificationValidates(Set(), Set(p3))

      Then("the result should be false")
      result should be (false)

    }
  }

  private def makeParameter = new Parameter("parameter", 1, 2, 0.01, null, false)
}
