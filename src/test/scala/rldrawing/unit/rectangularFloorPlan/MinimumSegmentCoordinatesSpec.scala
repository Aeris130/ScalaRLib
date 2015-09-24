package rldrawing.unit.rectangularFloorPlan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.RoomArea
import net.cyndeline.scalarlib.rldrawing.util.{Geom, Point}
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.MinimumSegmentCoordinates
import net.cyndeline.scalarlib.rldrawing.util.Direction._

@RunWith(classOf[JUnitRunner])
class MinimumSegmentCoordinatesSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

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
