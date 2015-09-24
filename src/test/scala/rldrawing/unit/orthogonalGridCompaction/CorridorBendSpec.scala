package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{RoomCorridorConnection, RectangularArea, CorridorBend}
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class CorridorBendSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  describe("CorridorBend") {

    it ("should specify area type") {

      Given("a bend")
      val area = RectangularArea(Point(0, 0), Point(3, 3))
      val bend = new CorridorBend(area, null, true)

      When("checking area type")
      Then("the type should be bend")
      bend.isRoom should be (false)
      bend.isBend should be (true)
      bend.isCorridor should be (false)

    }

    it ("should throw an exception when connecting a corridor opposite of another connection") {

      Given("a corridor bend")
      val area = RectangularArea(Point(0, 0), Point(3, 3))
      val bend = new CorridorBend(area, null, true)

      When("attempting to add a connection and another connection opposite to it")
      val c1 = mock[RoomCorridorConnection]
      val c2 = mock[RoomCorridorConnection]

      Then("an error should be thrown")
      intercept[Error] {
        bend.connect(North, c1)
        bend.connect(South, c2)
      }

    }
  }
}
