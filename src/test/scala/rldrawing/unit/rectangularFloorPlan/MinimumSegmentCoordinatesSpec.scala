package rldrawing.unit.rectangularFloorPlan

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{Geom, Point}
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.RoomArea
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.MinimumSegmentCoordinates
import testHelpers.SpecImports

class MinimumSegmentCoordinatesSpec extends SpecImports {

  describe("MinimumSegmentCoordinates") {

    it ("should use the coordinates of the opposite side if no intersections are available on the parallel sides") {

      Given("an area")
      val area = RoomArea(1, Point(2, 3), Point(9, 7))

      When("computing the coordinates for the left/right side using no intersections")
      val coordinates = MinimumSegmentCoordinates.minCoordinatesForSide(false, Vector(), area)

      Then("the upper and lower coordinates of the area should be returned instead")
      coordinates._1 should be (Geom.furthestCoordinate(South, area))
      coordinates._2 should be (Geom.furthestCoordinate(North, area))

    }

  }
}
