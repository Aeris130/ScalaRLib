package rldrawing.unit.rectangularFloorPlan

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{Geom, Intersection}
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.RectangleScaler
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.{FloorIntersection, RoomArea}
import net.cyndeline.scalarlib.rldrawing.util.Connection
import testHelpers.SpecImports

class RectangleScalerSpec extends SpecImports {
  private val scaler = new RectangleScaler(99.0, 1) // Large aspect ratio to prevent test interference
  private val defaultStartCoord = 5 // Gives some space to subtract without ending up with negative coordinates

  describe("RectangleScaler") {

    it("should scale a room connected to a single neighbor") {

      Given("a 4x4 room with a single intersection on the west side")
      val room = constructRoom(0, defaultStartCoord, 4, 4)
      val interStart = defaultStartCoord
      val interStop = Geom.furthestCoordinate(South, room)
      val intersection = createIntersection(West, room, interStart, interStop)

      When("reducing the room with a target size of 3x3")
      val reduced = scaler.scaleDown(room, 3*3, Vector(intersection)).get

      Then("the area should be 9")
      Geom.area(reduced.start, reduced.stop) should be (3*3)

    }

    it ("should only scale a side until it hits the inner coordinate of the leftmost and rightmost intersection") {

      Given("a room with height 3 and width 20, with two intersections each on the north and south sides")
      val room = constructRoom(0, defaultStartCoord, 20, 3)

      // Out of these intersections, the northern right and southern left constitutes the innermost coordinates that
      // limits the reduction. The west/east sides can't pass coordinate 13 from the right, and 10 from the left.
      val leftNorthIntersection = createIntersection(North, room, 9, 11)
      val rightNorthIntersection = createIntersection(North, room, 13, 16)

      val leftSouthIntersection = createIntersection(South, room, 7, 10)
      val rightSouthIntersection = createIntersection(South, room, 12, 14)

      val intersections = Vector(leftNorthIntersection, rightNorthIntersection, leftSouthIntersection, rightSouthIntersection)

      When("reducing the room with a target size that is smaller than what can be achieved given the intersecting constraints")
      val reduced = scaler.scaleDown(room, 4, intersections).get

      Then("the top and bottom y coordinates should remain as-is")
      reduced.start.y == room.start.y
      reduced.stop.y == room.stop.y

      And("the west segment should be moved to x coordinate 8 (10 - 2 coordinate margin)")
      reduced.start.x should be (8)

      And("the east segment should be moved to x coordinate 15 (13 + 2 coordinate margin)")
      reduced.stop.x should be (15)

    }

    it ("should not reduce an entry area that is limited by other areas outer coordinate") {

      Given("a room with larger size than its target")
      val room = constructRoom(0, defaultStartCoord, 10, 10)

      // An intersection covering the north side, just to prevent every side from being reducible.
      val interStart = defaultStartCoord
      val interStop = Geom.furthestCoordinate(East, room)
      val intersection = createIntersection(North, room, interStart, interStop)

      When("reducing the size of the room and marking that the southern side cannot be reduced below its current coordinate")
      val reduced = scaler.scaleDownEntry(room, 5, Vector(intersection), Vector(South -> Geom.furthestCoordinate(South, room))).get

      Then("the southern segment should remain unchanged")
      reduced.stop.y should be (room.stop.y)

    }

    it ("should return None if all 4 sides has neighbors") {

      Given("a room with intersections on all sides")
      val room = constructRoom(0, defaultStartCoord, 10, 10)
      val northIntersection = createIntersection(North, room, 6, 9)
      val southIntersection = createIntersection(South, room, 6, 9)
      val westIntersection = createIntersection(West, room, 6, 9)
      val eastIntersection = createIntersection(East, room, 6, 9)
      val intersections = Vector(northIntersection, southIntersection, westIntersection, eastIntersection)

      When("reducing a room to a size below the current one")
      val reduced = scaler.scaleDown(room, 4, intersections)

      Then("the result should be None")
      reduced should be (None)

    }

    it ("should return None if the current room already meets its target size") {

      Given("a room with size 9")
      val room = constructRoom(0, defaultStartCoord, 3, 3)
      val interStart = defaultStartCoord
      val interStop = Geom.furthestCoordinate(South, room)
      val intersection = createIntersection(West, room, interStart, interStop)

      When("reducing a room to size 9")
      val reduced = scaler.scaleDown(room, 9, Vector(intersection))

      Then("the result should be None")
      reduced should be (None)

    }

    it ("should stop before reaching the target size if moving one of the available segments causes the size to go from " +
        "larger than the target to smaller") {

      Given("a room with width 5 and height 10 (giving it size 50), with the shorter sides both having neighbors")
      val room = constructRoom(0, defaultStartCoord, 5, 10)
      val northIntersection = createIntersection(North, room, defaultStartCoord, Geom.furthestCoordinate(East, room))
      val southIntersection = createIntersection(South, room, defaultStartCoord, Geom.furthestCoordinate(South, room))

      When("reducing the room to target size 45")
      val reduced = scaler.scaleDown(room, 45, Vector(northIntersection, southIntersection))

      Then("no reduction should be possible, as doing so would cause the size to become 40")
      reduced should be (None)

    }

  }

  private def constructRoom(id: Int, startCoord: Int, width: Int, height: Int) = {
    val cs = Geom.areaCoordinates(defaultStartCoord, width, height)
    RoomArea(id, cs._1, cs._2)
  }

  private def createIntersection(d: Direction, owner: RoomArea[Int], from: Int, to: Int): FloorIntersection[Int] = {
    val innerSide = Geom.furthestCoordinate(d, owner.start, owner.stop)
    val outerSide = d match {
      case North | West => innerSide - 2
      case South | East => innerSide + 2
    }

    // Dummy neighbor coordinates
    val startX = d match {
      case North => from
      case South => from
      case West => outerSide
      case East => innerSide
    }
    val startY = d match {
      case North => outerSide
      case South => innerSide
      case West => from
      case East => from
    }
    val stopX = d match {
      case North => to
      case South => to
      case West => innerSide
      case East => outerSide
    }
    val stopY = d match {
      case North => innerSide
      case South => outerSide
      case West => to
      case East => to
    }

    val dummyNeighbor = RoomArea(owner.originalRoom + 1, Point(startX, startY), Point(stopX, stopY))
    val intersection = Intersection(owner, dummyNeighbor)
    FloorIntersection(d, dummyNeighbor, Connection(intersection))
  }

}
