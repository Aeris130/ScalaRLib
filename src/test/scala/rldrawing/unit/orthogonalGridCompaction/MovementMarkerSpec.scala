package rldrawing.unit.orthogonalGridCompaction

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.help.MovementMarker
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{Area, RectangularArea, RoomCorridorConnection, MutableArea}
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.{Point, Direction}

@RunWith(classOf[JUnitRunner])
class MovementMarkerSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  def targetWithAreaSouthwest = new {
    val target = Point(6, 2)
    val area = makeArea((0, 3), (4, 7))
  }

  def areaWithTargetWithin = new {
    val target = Point(5, 5)
    val area = makeArea((2, 2), (7, 7))
  }
  describe("MovementMarker") {

    it ("should mark an area to move if the area doesn't have the target inside it on the x axis") {

      Given("a marker with a target and an area to the southwest of it")
      val f = targetWithAreaSouthwest
      import f._
      val marker = createMarker(target)

      When("marking the area to move east")
      val status = marker.markAreasForMovement(Set(area), East)

      Then("the area should be marked to move east")
      area.movement should be (Some(East))

      And("the process status should be true")
      status should be (true)

    }

    it ("should mark an area to move if the area doesn't have the target inside it on the y axis") {

      Given("a marker with a target and an area to the southwest of it")
      val f = targetWithAreaSouthwest
      import f._
      val marker = createMarker(target)

      When("marking the area to move north")
      val status = marker.markAreasForMovement(Set(area), North)

      Then("the area should be marked to move north")
      area.movement should be (Some(North))

      And("the process status should be true")
      status should be (true)

    }

    it ("should not mark an area to move away from the target on the x axis") {

      Given("a marker with a target an an area that has the target inside its x axis")
      val target = Point(5, 5)
      val area = makeArea((4, 6), (7, 7))
      val marker = createMarker(target)

      When("marking the area to move east")
      val status = marker.markAreasForMovement(Set(area), East)

      Then("the area should not be marked")
      area.movement should be ('empty)

      And("the status should be false")
      status should be (false)

    }

    it ("should not mark an area to move on the y axis if the target x coordinate is within the area on that axis") {

      Given("a marker with a target an an area that has the target inside its y axis")
      val target = Point(5, 5)
      val area = makeArea((2, 2), (4, 7))
      val marker = createMarker(target)

      When("marking the area to move south")
      val status = marker.markAreasForMovement(Set(area), South)

      Then("the area should not be marked")
      area.movement should be ('empty)

      And("the status should be false")
      status should be (false)

    }

    it ("should not mark an area to move on the x axis if the target is within that area on both axises") {

      Given("a marker with a target and an area overlapping the target")
      val f = areaWithTargetWithin
      import f._
      val marker = createMarker(target)

      When("marking the area to move west")
      val status = marker.markAreasForMovement(Set(area), West)

      Then("the area should not be marked")
      area.movement should be ('empty)

      And("the status should be false")
      status should be (false)

    }

    it ("should not mark an area to move on the y axis if the target is within that area on both axises") {

      Given("a marker with a target and an area overlapping the target")
      val f = areaWithTargetWithin
      import f._
      val marker = createMarker(target)

      When("marking the area to move north")
      val status = marker.markAreasForMovement(Set(area), North)

      Then("the area should not be marked")
      area.movement should be ('empty)

      And("the status should be false")
      status should be (false)

    }

    it ("should return false if a single area in the set has reached the target") {

      Given("a marker with a target and 3 areas, one of which has reached the target")
      val f = areaWithTargetWithin
      import f._
      val marker = createMarker(target)
      val areaReached = area
      val area2 = makeArea((10, 10), (12, 12))
      val area3 = makeArea((20, 20), (22, 22))

      When("marking the areas to move north")
      val status = marker.markAreasForMovement(Set(areaReached, area2, area3), North)

      Then("the status should be false")
      status should be (false)

    }

    it ("should move corridors even though they have the target in them") {

      Given("a marker with a target and a corridor that overlaps the target")
      val target = Point(5, 5)
      val marker = createMarker(target)
      val corridor = makeArea((2, 2), (7, 7), true)

      When("marking the corridor to move east")
      val status = marker.markAreasForMovement(Set(corridor), East)

      Then("the corridor should be marked to move east")
      corridor.movement should be (Some(East))

      Then("the status should be true")
      status should be (true)

    }

    it ("should not allow western movement if a room has x coordinate 0") {

      Given("a marker with an arbitrary target and an area with x coordinate 0")
      val marker = createMarker(Point(99, 99))
      val room = makeArea((0, 3), (5, 5), false)

      When("moving the corridor west")
      val status = marker.markAreasForMovement(Set(room), West)

      Then("the status should be false")
      status should be (false)

    }

    it ("should not allow northern movement if a room has y coordinate 0") {

      Given("a marker with an arbitrary target and an area with y coordinate 0")
      val marker = createMarker(Point(99, 99))
      val room = makeArea((3, 0), (5, 5), false)

      When("moving the corridor west")
      val status = marker.markAreasForMovement(Set(room), North)

      Then("the status should be false")
      status should be (false)

    }

    it ("should not allow eastern movement if a room has an x coordinate equal to the maximum value") {

      Given("a marker with max 99, an arbitrary target and an area with x coordinate 99")
      val marker = createMarker(Point(0, 0))
      val room = makeArea((3, 3), (99, 7), false)

      When("moving the corridor west")
      val status = marker.markAreasForMovement(Set(room), East)

      Then("the status should be false")
      status should be (false)

    }

    it ("should not allow southern movement if a room has an y coordinate equal to the maximum value") {

      Given("a marker with max 99, an arbitrary target and an area with x coordinate 99")
      val marker = createMarker(Point(0, 0))
      val room = makeArea((3, 3), (7, 99), false)

      When("moving the corridor west")
      val status = marker.markAreasForMovement(Set(room), South)

      Then("the status should be false")
      status should be (false)

    }

  }

  /** Creates a marker with a grid of size 100. */
  private def createMarker(target: Point) = new MovementMarker(target, Point(0, 0), Point(99, 99))

  private def makeArea(start: (Int, Int), stop: (Int, Int), corridor: Boolean = false) = {
    class TestArea extends Area(new RectangularArea(Point(start._1, start._2), Point(stop._1, stop._2)), false) {
      def isCorridor: Boolean = corridor

      // Don't need this for this test
      def isRoom: Boolean = ???
      def isBend: Boolean = ???
      def canMove(direction: Direction.Direction): Set[MutableArea] = ???
      def move(): Unit = ???
    }

    new TestArea()
  }
}
