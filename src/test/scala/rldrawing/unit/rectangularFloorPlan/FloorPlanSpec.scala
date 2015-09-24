package rldrawing.unit.rectangularFloorPlan

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Point
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.FloorPlan
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.ModifiedCoordinates
import rldrawing.help.ConstraintRoom
import testHelpers.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class FloorPlanSpec extends SpecImports {

  /**
   * Two rooms of size 2x2 between (0,0)(1,1) and(1,0)(2,1).
   */
  private def leftRightRoom(r1: ConstraintRoom, r2: ConstraintRoom): RectangularLayout[ConstraintRoom, UnDiEdge] =  {
    rectLayout((r1, (0, 0), (1, 1)), (r2, (1, 0), (2, 1)))
  }

  /** 2 3x2 rooms at (0,0)->(2,1) and (2,0)->(4,1). */
  private def asymmetricRoomsLeftRight = new {
    val room1 = ConstraintRoom(3, 2)
    val room2 = ConstraintRoom(3, 2)
    val layout = rectLayout((room1, (0, 0), (2, 1)), (room2, (2, 0), (4, 1)))
  }
  
  /** 2 2x3 rooms on top of each other. */
  private def asymmetricRoomsTopDown = new {
    val room1 = ConstraintRoom(2, 3)
    val room2 = ConstraintRoom(2, 3)
    val layout = rectLayout((room1, (0, 0), (1, 2)), (room2, (0, 2), (1, 4)))
  }

  /** 2 3x2 rooms sharing a gate between them. */
  private def roomsWithGatebetween = new {
    val room1 = ConstraintRoom(3, 2)
    val room2 = ConstraintRoom(3, 2)
    val area1 = (room1, (0, 0), (2, 2))
    val gate = ((room1, room2), (2, 0), (3,2))
    val area2 = (room2, (3, 0), (5,2))
    val graph = Graph(room1~room2)

    val layout = rectLayoutWithGates(area1, area2)(gate)(graph)
  }

  describe("FloorPlan") {

    /*
     * Misc
     */

    it ("should store the maximum x/y coordinate") {

      Given("two scaled rooms")
      val r1 = ConstraintRoom(12, 22)
      val r2 = ConstraintRoom(23, 54)
      val layout = leftRightRoom(r1, r2)

      When("converting the layout to a floor plan")
      val floorPlan = FloorPlan(layout)

      Then("the stop coordinates of room 2 should be the highest x/y value")
      val roomArea2 = findRoom(r2, floorPlan)
      floorPlan.maxX should be (roomArea2.stop.x)
      floorPlan.maxY should be (roomArea2.stop.y)

    }

    it ("should store the minimum x/y coordinate") {

      Given("two rooms with start x/y coordinates > 0")
      val r1 = ConstraintRoom(12, 22)
      val r2 = ConstraintRoom(23, 54)
      val layout = rectLayout((r1, (1, 2), (3, 3)), (r2, (3, 2), (4, 3)))

      When("converting the layout to a floor plan")
      val floorPlan = FloorPlan(layout)

      Then("the start coordinates of room 1 should be the lowest x/y value")
      val roomArea1 = findRoom(r1, floorPlan)
      floorPlan.minX should be (roomArea1.start.x)
      floorPlan.minY should be (roomArea1.start.y)

    }

    /*
     * Rotations
     */

    it ("should rotate a room 90 degrees clockwise around the point (0, 0)") {

      Given("a room of size 3x2 with coordinates (0,0)->(2,1)")
      val room = ConstraintRoom(3, 2)
      val layout = rectLayout((room, (0, 0), (2, 1)))

      When("rotating the floor plan clockwise")
      val floorPlan = FloorPlan(layout).rotate90Clockwise

      Then("the room should have coordinates (0,0)->(1,2)")
      val rotatedArea = findRoom(room, floorPlan)
      rotatedArea.start should be (Point(0, 0))
      rotatedArea.stop should be (Point(1, 2))

    }

    it ("should rotate a room that isn't adjacent to the rotating point (0, 0) clockwise") {

      Given("two rooms of size 3x2 at points (0,0)->(2,1) and (2,0)->(4,1)")
      val f = asymmetricRoomsLeftRight
      import f._

      When("rotating the floor plan clockwise")
      val floorPlan = FloorPlan(layout).rotate90Clockwise

      Then("room 2 should have coordinates (0, 2)->(1,4)")
      val rotatedArea = findRoom(room2, floorPlan)
      rotatedArea.start should be (Point(0, 2))
      rotatedArea.stop should be (Point(1, 4))

    }

    it ("should rotate a room that isn't adjacent to the rotating point (0, 0) counter clockwise") {

      Given("two rooms of size 3x2 at points (0,0)->(2,1) and (2,0)->(4,1)")
      val f = asymmetricRoomsLeftRight
      import f._

      When("rotating the floor plan counter clockwise")
      val floorPlan = FloorPlan(layout).rotate90CounterClockwise

      Then("room 1 should have coordinates (0, 2)->(1,4)")
      val rotatedArea = findRoom(room1, floorPlan)
      rotatedArea.start should be (Point(0, 2))
      rotatedArea.stop should be (Point(1, 4))

    }

    it ("should rotate 360 degrees using 90 degree clockwise increments") {

      Given("two rooms of differing height/width")
      val f = asymmetricRoomsLeftRight
      import f._

      When("rotating the floor plan counter clockwise 4 times and clockwise 4 times")
      val floorPlan = FloorPlan(layout).rotate90Clockwise.rotate90Clockwise.rotate90Clockwise.rotate90Clockwise

      Then("room 1 should retain its coordinates")
      val rotatedArea1 = findRoom(room1, floorPlan)
      rotatedArea1.start should be (Point(0, 0))
      rotatedArea1.stop should be (Point(2, 1))

      Then("room 2 should retain its coordinates")
      val rotatedArea2 = findRoom(room2, floorPlan)
      rotatedArea2.start should be (Point(2, 0))
      rotatedArea2.stop should be (Point(4, 1))

    }

    it ("should rotate 360 degrees using 90 degree counter clockwise increments") {

      Given("two rooms of differing height/width")
      val f = asymmetricRoomsLeftRight
      import f._

      When("rotating the floor plan counter clockwise 4 times and clockwise 4 times")
      val floorPlan = FloorPlan(layout).rotate90CounterClockwise.rotate90CounterClockwise.rotate90CounterClockwise.rotate90CounterClockwise

      Then("room 1 should retain its coordinates")
      val rotatedArea1 = findRoom(room1, floorPlan)
      rotatedArea1.start should be (Point(0, 0))
      rotatedArea1.stop should be (Point(2, 1))

      Then("room 2 should retain its coordinates")
      val rotatedArea2 = findRoom(room2, floorPlan)
      rotatedArea2.start should be (Point(2, 0))
      rotatedArea2.stop should be (Point(4, 1))

    }

    it ("should adjust a clockwise rotated room within positive coordinates to the rotation point when the point isn't (0,0)") {

      Given("two rooms of size 3x2 at points (3,2)->(5,3) and (5,2)->(7,3)")
      val room1 = ConstraintRoom(3, 2)
      val room2 = ConstraintRoom(3, 2)
      val layout = rectLayout((room1, (3, 2), (5, 3)), (room2, (5, 2), (7, 3)))

      When("rotating the floor plan clockwise")
      val floorPlan = FloorPlan(layout).rotate90Clockwise

      Then("room 1 should have coordinates (3,2)->(4,4)")
      val rotatedArea1 = findRoom(room1, floorPlan)
      rotatedArea1.start should be (Point(3, 2))
      rotatedArea1.stop should be (Point(4, 4))

    }

    it ("should adjust a counter clockwise rotated room within positive coordinates to the rotation point when the point isn't (0,0)") {

      Given("two rooms of size 3x2 at points (3,6)->(5,7) and (5,6)->(7,7)")
      val room1 = ConstraintRoom(3, 2)
      val room2 = ConstraintRoom(3, 2)
      val layout = rectLayout((room1, (3, 6), (5, 7)), (room2, (5, 6), (7, 7)))

      When("rotating the floor plan clockwise")
      val floorPlan = FloorPlan(layout).rotate90CounterClockwise

      Then("room 2 should have coordinates (3,6)->(4,8)")
      val rotatedArea2 = findRoom(room2, floorPlan)
      rotatedArea2.start should be (Point(3, 6))
      rotatedArea2.stop should be (Point(4, 8))

    }

    it ("should adjust a clockwise rotated room within negative coordinates to the rotation point when the point isn't (0,0)") {

      Given("two rooms of size 3x2 at points (3,6)->(5,7) and (5,6)->(7,7)")
      val room1 = ConstraintRoom(3, 2)
      val room2 = ConstraintRoom(3, 2)
      val layout = rectLayout((room1, (3, 6), (5, 7)), (room2, (5, 6), (7, 7)))

      When("rotating the floor plan twice, causing the rooms to enter the negative side of the coordinate system")
      val floorPlan = FloorPlan(layout).rotate90Clockwise.rotate90Clockwise

      Then("room 2 should have coordinates (3,6)->(5,7)")
      val rotatedArea2 = findRoom(room2, floorPlan)
      rotatedArea2.start should be (Point(3, 6))
      rotatedArea2.stop should be (Point(5, 7))

      And("room 1 should have coordinates (5,6)->(7,7)")
      val rotatedArea1 = findRoom(room1, floorPlan)
      rotatedArea1.start should be (Point(5, 6))
      rotatedArea1.stop should be (Point(7, 7))

    }

    it ("should adjust a counter clockwise rotated room within negative coordinates to the rotation point when the point isn't (0,0)") {

      Given("two rooms of size 3x2 at points (3,6)->(5,7) and (5,6)->(7,7)")
      val room1 = ConstraintRoom(3, 2)
      val room2 = ConstraintRoom(3, 2)
      val layout = rectLayout((room1, (3, 2), (5, 3)), (room2, (5, 2), (7, 3)))

      When("rotating the floor plan counter clockwise, causing the rooms to enter the negative side of the coordinate system")
      val floorPlan = FloorPlan(layout).rotate90CounterClockwise.rotate90CounterClockwise

      Then("room 2 should have coordinates (3,2)->(5,3)")
      val rotatedArea2 = findRoom(room2, floorPlan)
      rotatedArea2.start should be (Point(3, 2))
      rotatedArea2.stop should be (Point(5, 3))

      And("room 1 should have coordinates (5,2)->(7,3)")
      val rotatedArea1 = findRoom(room1, floorPlan)
      rotatedArea1.start should be (Point(5, 2))
      rotatedArea1.stop should be (Point(7, 3))

    }

    it ("should adjust the coordinates equally when rotating a drawing counter clockwise thrice as when rotating clockwise once") {

      Given("two rooms of differing height/width")
      val f = asymmetricRoomsLeftRight
      import f._

      When("rotating one floor plan counter clockwise thrice, and another clockwise once")
      val floorPlan1 = FloorPlan(layout).rotate90CounterClockwise.rotate90CounterClockwise.rotate90CounterClockwise
      val floorPlan2 = FloorPlan(layout).rotate90Clockwise

      Then("the rooms in both drawings should have the same coordinates")
      val ccwArea2 = findRoom(room2, floorPlan1)
      val cwArea2 = findRoom(room2, floorPlan2)
      ccwArea2.start should equal (cwArea2.start)
      ccwArea2.stop should equal (cwArea2.stop)

    }
    
    it ("should reverse the y axis of a drawing") {
      
      Given("two asymmetric rooms on top of each other")
      val f = asymmetricRoomsTopDown
      import f._
      
      When("flipping the y axis of the drawing")
      val floorPlan = FloorPlan(layout)
      val flippedFloorPlan = floorPlan.flipYAxis

      Then("room 1 should have the old coordinates of room 2")
      val oldArea2 = findRoom(room2, floorPlan)
      val flippedArea1 = findRoom(room1, flippedFloorPlan)
      flippedArea1.start should equal (oldArea2.start)
      flippedArea1.stop should equal (oldArea2.stop)

      And("room 2 should have the old coordinates of room 1")
      val oldArea1 = findRoom(room1, floorPlan)
      val flippedArea2 = findRoom(room2, flippedFloorPlan)
      flippedArea2.start should equal (oldArea1.start)
      flippedArea2.stop should equal (oldArea1.stop)

    }

    /*
     * Gates
     */

    it ("should create gate areas") {

      Given("a two rooms that connect to a common gate at points (2,0)(2,2) and (3,0)(3,2)")
      val f = roomsWithGatebetween
      import f._

      When("converting the layout to a floor plan")
      val floorPlan = FloorPlan(layout)

      Then("the resulting plan should contain a gate with coordinates (2, 0), (3, 2)")
      assert(floorPlan.roomAreas.exists(_.isGate), "No gate found in floor plan " + floorPlan)
      val gate = floorPlan.roomAreas.find(_.isGate).get
      gate.start should be (Point(2, 0))
      gate.stop should be (Point(3, 2))

    }

    /*
     * Intersections
     */

    it ("should compute an intersection between two neighbors") {

      Given("two rooms that intersects at points (5,2) and (5,3)")
      val leftArea = ConstraintRoom(4, 3)
      val rightArea = ConstraintRoom(4, 3)
      val layout = rectLayout(Graph(leftArea~rightArea), (leftArea, (3, 2), (6, 4)), (rightArea, (6, 2), (8, 4)))

      When("converting the layout to a floor plan")
      val floorPlan = FloorPlan(layout)
      val leftRoom = findRoom(leftArea, floorPlan)
      val rightRoom = findRoom(rightArea, floorPlan)

      And("the left room should have an eastern intersection with the right room")
      val leftIntersect = floorPlan.intersections(leftRoom)
      leftIntersect should have size 1
      leftIntersect.head.direction should be (East)
      leftIntersect.head.neighbor should be (rightRoom)
      leftIntersect.head.connection.start should be (Point(6, 2))
      leftIntersect.head.connection.stop should be (Point(6, 4))

      And("the right room should have a western intersection with the left room")
      val rightIntersect = floorPlan.intersections(rightRoom)
      rightIntersect should have size 1
      rightIntersect.head.direction should be (West)
      rightIntersect.head.neighbor should be (leftRoom)
      rightIntersect.head.connection.start should be (Point(6, 2))
      rightIntersect.head.connection.stop should be (Point(6, 4))

    }

    it ("should compute intersections between the neighbors of a gate") {

      Given("a two room areas that connect to a common gate at points (2,0)(2,2) and (3,0)(3,2)")
      val f = roomsWithGatebetween
      import f._

      When("converting the layout to a floor plan")
      val floorPlan = FloorPlan(layout)
      val leftRoom = findRoom(room1, floorPlan)
      val rightRoom = findRoom(room2, floorPlan)
      val gate = floorPlan.roomAreas.find(_.isGate).get

      Then("the left area should have an intersection between (2,0) and (2,2)")
      val a1Intersect = floorPlan.intersections(leftRoom).head
      a1Intersect.neighbor should be (gate)
      a1Intersect.direction should be (East)
      a1Intersect.connection.start should be (Point(2, 0))
      a1Intersect.connection.stop should be (Point(2, 2))

      And("the right area should have an intersection between (3,0) and (3,2)")
      val a2Intersect = floorPlan.intersections(rightRoom).head
      a2Intersect.neighbor should be (gate)
      a2Intersect.direction should be (West)
      a2Intersect.connection.start should be (Point(3, 0))
      a2Intersect.connection.stop should be (Point(3, 2))

    }

    it ("should throw an exception if two regular rooms overlap") {

      Given("a layout with three rooms, where two non-neighbors overlap")
      val room1 = ConstraintRoom(3, 3)
      val room2 = ConstraintRoom(3, 3)
      val room3 = ConstraintRoom(3, 3) // overlaps with 2
      val layout = rectLayout((room1, (0, 0), (4, 1)), (room2, (0, 1), (3, 2)), (room3, (2, 1), (4, 2)))

      When("converting the layout to a floor plan")
      Then("an exception should be thrown")
      intercept[Error] {
        FloorPlan(layout)
      }

    }

    it ("should thrown an exception if a room and a gate overlap") {

      Given("a layout with two rooms and one gate, where two non-neighbors overlap")
      val room1 = ConstraintRoom(3, 3)
      val gate = ConstraintRoom(3, 3)
      val room3 = ConstraintRoom(3, 3) // overlaps with gate
      val layout = rectLayout((room1, (0, 0), (3, 1)), (gate, (3, 0), (5, 1)), (room3, (4, 0), (7, 1)))

      When("converting the layout to a floor plan")
      Then("an exception should be thrown")
      intercept[Error] {
        FloorPlan(layout)
      }

    }

    it ("should throw an exception if two neighbors only intersect at a single coordinate") {

      Given("a layout with two neighbors that intersect at coordinate (1, 1)")
      val room1 = ConstraintRoom(2, 2)
      val room2 = ConstraintRoom(2, 2)
      val graph = Graph(room1~room2)
      val layout = rectLayout(graph, (room1, (0, 0), (3, 3)), (room2, (3, 2), (5, 5)))

      When("converting the layout to a floor plan")
      Then("an exception should be thrown")
      intercept[Error] {
        FloorPlan(layout)
      }

    }

    it ("should throw an exception if a room and a gate intersects at only 2 coordinates") {

      Given("a layout with two rooms and one gate, the rooms connect with the gate at a single coordinate")
      val room1 = ConstraintRoom(3, 3)
      val room2 = ConstraintRoom(3, 3) // overlaps with gate
      val gateEntry = ((room1, room2), (3, 2), (4, 4))
      val layout = rectLayoutWithGates((room1, (0, 0), (3, 3)), (room2, (4, 0), (7, 3)))(gateEntry)(Graph(room1~room2))

      When("converting the layout to a floor plan")
      Then("an exception should be thrown")
      intercept[Error] {
        FloorPlan(layout)
      }

    }

    /*
     * Coordinate modification
     */

    // Note that this test doesn't care about adjacencies
    it ("should adjust the coordinates of a room") {

      Given("a room with coordinates (2,0)->(4,1)")
      val f = asymmetricRoomsLeftRight
      import f._
      val room = room2

      When("updating its coordinates with (3,2)->(5,4)")
      val coordinates = ModifiedCoordinates(Point(3, 2), Point(5, 4))
      val floorPlan = FloorPlan(layout)
      val roomArea = findRoom(room, floorPlan)
      val modifiedFloorPlan = FloorPlan(layout).updateRooms(Vector(roomArea -> coordinates))

      Then("the room area should have coordinates (3,2)->(5,4)")
      val updatedRoomArea = findRoom(room, modifiedFloorPlan)
      updatedRoomArea.start should be (Point(3, 2))
      updatedRoomArea.stop should be (Point(5, 4))

    }

    it ("should adjust the coordinates of two rooms, where the first adjustment will cause the rooms to be separated " +
        "until the second adjustment is performed") {

      Given("a room (5,5)->(9,9) adjacent to (9,5)->(14,9)")
      val room1 = ConstraintRoom(5, 5)
      val room2 = ConstraintRoom(5, 5)
      val r1Start = (5, 5)
      val r1Stop = (9, 9)
      val r2Start = (9, 5)
      val r2Stop = (14, 9)
      val layout = rectLayout(Graph(room1~room2), (room1, r1Start, r1Stop), (room2, r2Start, r2Stop))
      val floorPlan = FloorPlan(layout)

      When("moving both rooms 20 coordinates down the y axis")
      val room1Update = ModifiedCoordinates(Point(r1Start._1, r1Start._2 + 20), Point(r1Stop._1, r1Stop._2 + 20))
      val room2Update = ModifiedCoordinates(Point(r2Start._1, r2Start._2 + 20), Point(r2Stop._1, r2Stop._2 + 20))
      val roomArea1 = findRoom(room1, floorPlan)
      val roomArea2 = findRoom(room2, floorPlan)

      // Exception here if disconnection between the rooms are detected
      val modifiedFloorPlan = floorPlan.updateRooms(Vector(roomArea1 -> room1Update, roomArea2 -> room2Update))

      Then("both rooms should have their y axis increased by 20")
      val updatedRoomArea1 = findRoom(room1, modifiedFloorPlan)
      val updatedRoomArea2 = findRoom(room2, modifiedFloorPlan)

      updatedRoomArea1.start.y should be (r1Start._2 + 20)
      updatedRoomArea1.stop.y should be (r1Stop._2 + 20)
      updatedRoomArea2.start.y should be (r2Start._2 + 20)
      updatedRoomArea2.stop.y should be (r2Stop._2 + 20)

    }

  }

  private def findRoom(r: ConstraintRoom, floorPlan: FloorPlan[ConstraintRoom, UnDiEdge]) = floorPlan.roomAreas.find(_.originalRoom == r).get

  private def rectLayout(rooms: (ConstraintRoom, (Int, Int), (Int, Int))*): RectangularLayout[ConstraintRoom, UnDiEdge]
    = new RectangularLayout(rooms.toVector, Vector(), Vector(), Graph[ConstraintRoom, UnDiEdge]())

  private def rectLayout(graph: Graph[ConstraintRoom, UnDiEdge], rooms: (ConstraintRoom, (Int, Int), (Int, Int))*): RectangularLayout[ConstraintRoom, UnDiEdge]
    = new RectangularLayout(rooms.toVector, Vector(), Vector(), graph)

  private def rectLayoutWithGates(rooms: (ConstraintRoom, (Int, Int), (Int, Int))*)(gates: ((ConstraintRoom, ConstraintRoom), (Int, Int), (Int, Int))*)(graph: Graph[ConstraintRoom, UnDiEdge]): RectangularLayout[ConstraintRoom, UnDiEdge]
    = new RectangularLayout(rooms.toVector, gates.toVector, Vector(), graph)
}
