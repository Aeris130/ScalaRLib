package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{CorridorBend, RectangularArea, RoomCorridorConnection}
import testHelpers.SpecImports

class CorridorBendSpec extends SpecImports {

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
