package util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.util.FrontInterval

@RunWith(classOf[JUnitRunner])
class FrontIntervalSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("FrontInterval") {

    it ("should split a list evenly into sublists") {

      Given("a list of size 8")
      val l = (1 to 8).toVector

      When("partitioning the list into 4 sublists")
      val sublists = FrontInterval(l, 4)

      Then("each sublist should contain 2 elements")
      sublists should equal (Vector(Vector(1,2), Vector(3,4), Vector(5,6), Vector(7,8)))

    }

    it ("should have the lowest amount of elements in the tail list if the remainder is less than the maximum element size per list") {

      Given("a list of size 7")
      val l = (1 to 7).toVector

      When("partitioning the list into 4 sublists")
      val sublists = FrontInterval(l, 4)

      Then("the last list should contain a single element")
      sublists should equal (Vector(Vector(1,2), Vector(3,4), Vector(5,6), Vector(7)))

    }

    it ("should divide multiple intervals of 1 between previous intervals") {

      Given("a list of size 6")
      val l = (1 to 6).toVector

      When("partitioning the list into 4 sublists")
      val sublists = FrontInterval(l, 4)

      Then("the first two intervals should contain an extra element")
      sublists should equal (Vector(Vector(1,2), Vector(3,4), Vector(5), Vector(6)))

    }

    it ("should divide an extra tail interval of size > 1 between the initial 4 intervals") {

      Given("a list of size 23")
      val l = (1 to 23).toVector

      When("partitioning the list into 4 sublists")
      val sublists = FrontInterval(l, 4)

      Then("the first 3 lists should contain 6 elements")
      sublists should equal (Vector((1 to 6).toVector, (7 to 12).toVector, (13 to 18).toVector, (19 to 23).toVector))

    }

    it ("should divide elements deterministically") {

      Given("a list of size 100")
      val l = (1 to 100).toVector

      When("partitioning the list into 5 sublists 100 times")
      val firstList = FrontInterval(l, 5)

      Then("every result should equal the first")
      for (i <- 0 to 100) {
        FrontInterval(l, 5) should equal (firstList)
      }

    }
  }
}
