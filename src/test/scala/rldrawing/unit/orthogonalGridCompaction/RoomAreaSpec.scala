package rldrawing.unit.orthogonalGridCompaction

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{RoomCorridorConnection, MutableArea, RectangularArea, RoomArea}
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.util.Point

@RunWith(classOf[JUnitRunner])
class RoomAreaSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  def gridRoom = new {
    val grid = mock[PartitionedArea[MutableArea]]
    val room = makeRoomWithGrid(Point(3, 3), Point(6, 7), grid)
  }

  describe("RoomArea") {

    it ("should specify area type") {

      Given("a room")
      val room = makeDefaultRoom

      When("checking area type")
      Then("the type should be room")
      room.isRoom should be (true)
      room.isBend should be (false)
      room.isCorridor should be (false)

    }

    it ("should throw an exception whe attempting to create a room with less than 3 width") {

      Given("a rectangular area with width 2")
      val area = RectangularArea(Point(1, 2), Point(2, 4))

      When("creating a new room")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new RoomArea(area, null, true)
      }

    }

    it ("should throw an exception whe attempting to create a room with less than 3 height") {

      Given("a rectangular area with height 2")
      val area = RectangularArea(Point(1, 2), Point(5, 3))

      When("creating a new room")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new RoomArea(area, null, true)
      }

    }

    it ("should supply start/stop coordinates based on its rectangular area") {

      Given("a room with area between (1, 2) and (3, 4)")
      val room = new RoomArea(RectangularArea(Point(1, 2), Point(3, 4)), null, true)

      When("retrieving start/stop values")
      val start = room.start
      val stop = room.stop

      Then("start and stop should equal the areas start/stop")
      start should equal (Point(1, 2))
      stop should equal (Point(3, 4))

    }

    it ("should mark itself as able to move") {

      Given("a room")
      val room = makeDefaultRoom

      When("marking the room as able to move in a direction")
      room.markAsMoved(East)

      Then("the room should be marked as able to move in that direction")
      room.movement should be ('defined)
      room.movement should equal (Some(East))

    }

    it ("should throw an error when attempting to mark a room for movement twice without resetting") {

      Given("a room")
      val room = makeDefaultRoom

      When("marking the room as able to move in a direction twice")
      Then("an error should be thrown")
      intercept[Error] {
        room.markAsMoved(East)
        room.markAsMoved(South)
      }

    }

    it ("should reset its own movement marking") {

      Given("a room")
      val room = makeDefaultRoom

      When("marking the room as able to move in a direction and then resetting it")
      room.markAsMoved(East)
      room.clearMovement()

      Then("no movement should be found in the room")
      room.movement should not be ('defined)

    }

    it ("should throw an error when attempting to reset a room with no movement mark") {

      Given("a room")
      val room = makeDefaultRoom

      When("resetting its movement without first marking it")
      Then("an error should be thrown")
      intercept[Error] {
        room.clearMovement()
      }

    }

    it ("should throw an error when moving if no movement is set") {

      Given("a room with no movement set")
      val room = makeDefaultRoom

      When("moving the room")
      Then("an error should be thrown")
      intercept[Error] {
        room.move
      }

    }

    it ("should move itself north") {

      Given("a room with area (3,3) to (6,7) that is registered to move north")
      val f = gridRoom
      import f._
      room.markAsMoved(North)

      (grid.add _) expects(*) returns() anyNumberOfTimes()
      (grid.remove _) expects(*) returns() anyNumberOfTimes()

      When("moving the room")
      room.move

      Then("the rooms y coordinates should be reduced by 1")
      room.area.start should be (Point(3, 2))
      room.area.stop should be (Point(6, 6))

    }

    it ("should move itself south") {

      Given("a room with area (3,3) to (6,7) that is registered to move south")
      val f = gridRoom
      import f._
      room.markAsMoved(South)

      (grid.add _) expects(*) returns() anyNumberOfTimes()
      (grid.remove _) expects(*) returns() anyNumberOfTimes()

      When("moving the room")
      room.move

      Then("the rooms y coordinates should be increased by 1")
      room.area.start should be (Point(3, 4))
      room.area.stop should be (Point(6, 8))

    }

    it ("should move itself west") {

      Given("a room with area (3,3) to (6,7) that is registered to move west")
      val f = gridRoom
      import f._
      room.markAsMoved(West)

      (grid.add _) expects(*) returns() anyNumberOfTimes()
      (grid.remove _) expects(*) returns() anyNumberOfTimes()

      When("moving the room")
      room.move

      Then("the rooms x coordinates should be reduced by 1")
      room.area.start should be (Point(2, 3))
      room.area.stop should be (Point(5, 7))

    }

    it ("should move itself east") {

      Given("a room with area (3,3) to (6,7) that is registered to move east")
      val f = gridRoom
      import f._
      room.markAsMoved(East)

      (grid.add _) expects(*) returns() anyNumberOfTimes()
      (grid.remove _) expects(*) returns() anyNumberOfTimes()

      When("moving the room")
      room.move

      Then("the rooms x coordinates should be increased by 1")
      room.area.start should be (Point(4, 3))
      room.area.stop should be (Point(7, 7))

    }

    it ("should updates its position in the grid when moving") {

      Given("a room with area (3,3) to (6,7) that is registered to move east")
      val f = gridRoom
      import f._
      room.markAsMoved(East)

      When("moving the room")
      Then("the grid should have the old room removed, and the updated one inserted")

      // Can't make equals depend on the mutable rectangle however, this will have to do.
      inSequence{
        (grid.remove _) expects(room) returns() once()
        (grid.add _) expects(room) returns() once()
      }

      // Start test
      room.move

    }

    /*
     * Checking adjacent areas when moving
     */

    it ("should return an empty set when asked to move into a direction with no connections and no colliding areas") {

      Given("a room with no connections and a grid with no colliding areas")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(3, 3), Point(5, 5), grid)

      (grid.elementsIn _) expects(*, *) returns(Set()) anyNumberOfTimes()

      When("asking if the room can move towards a target")
      val obstructingAreas = room.canMove(South)

      Then("an empty list should be returned")
      obstructingAreas should be ('empty)

    }

    it ("should not return itself") {

      Given("two rooms A and B and a grid that returns both when room A checks for adjacent areas to its own position")
      val grid = mock[PartitionedArea[MutableArea]]
      val roomA = makeRoomWithGrid(Point(3, 3), Point(5, 5), grid)
      val roomB = makeRoomWithGrid(Point(3, 0), Point(5, 3), grid)
      (grid.elementsIn _) expects(*, *) returns(Set(roomA, roomB)) once()

      When("asking if A can move towards B")
      val obstructingAreas = roomA.canMove(North)

      Then("only room B should be returned")
      obstructingAreas should be (Set(roomB))

    }

    it ("should return a set with the northern corridor when asked to move north if there's a northern corridor connection that must be moved") {

      Given("a room with a northern corridor connection that must be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val corridor = makeMockCorridor
      val connection = mock[RoomCorridorConnection]
      room.connect(North, connection)
      (connection.roomCanMoveIndependently _) expects(North) returns(false) once()
      (connection.corridor _) expects() returns(corridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move north")
      val areas = room.canMove(North)

      Then("the corridor connected to the room should be returned")
      areas should be (Set(corridor))

    }

    it ("should return a set with the southern corridor when asked to move south if there's a southern corridor connection that must be moved") {

      Given("a room with a southern corridor connection that must be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val corridor = makeMockCorridor
      val connection = mock[RoomCorridorConnection]
      room.connect(South, connection)
      (connection.roomCanMoveIndependently _) expects(South) returns(false) once()
      (connection.corridor _) expects() returns(corridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move south")
      val areas = room.canMove(South)

      Then("the corridor connected to the room should be returned")
      areas should be (Set(corridor))

    }

    it ("should return a set with the western corridor when asked to move west if there's a western corridor connection that must be moved") {

      Given("a room with a western corridor connection that must be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val corridor = makeMockCorridor
      val connection = mock[RoomCorridorConnection]
      room.connect(West, connection)
      (connection.roomCanMoveIndependently _) expects(West) returns(false) once()
      (connection.corridor _) expects() returns(corridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move west")
      val areas = room.canMove(West)

      Then("the corridor connected to the room should be returned")
      areas should be (Set(corridor))

    }

    it ("should return a set with the eastern corridor when asked to move east if there's a eastern corridor connection that must be moved") {

      Given("a room with a eastern corridor connection that must be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val corridor = makeMockCorridor
      val connection = mock[RoomCorridorConnection]
      room.connect(East, connection)
      (connection.roomCanMoveIndependently _) expects(East) returns(false) once()
      (connection.corridor _) expects() returns(corridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("the corridor connected to the room should be returned")
      areas should be (Set(corridor))

    }

    it ("should return a set of all corridors when there's a corridor in every direction that must be moved") {

      Given("a room with 4 connections that must be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val northCorridor = makeMockCorridor
      val southCorridor = makeMockCorridor
      val westCorridor = makeMockCorridor
      val eastCorridor = makeMockCorridor
      val northConnection = mock[RoomCorridorConnection]
      val southConnection = mock[RoomCorridorConnection]
      val westConnection = mock[RoomCorridorConnection]
      val eastConnection = mock[RoomCorridorConnection]
      room.connect(North, northConnection)
      room.connect(South, southConnection)
      room.connect(West, westConnection)
      room.connect(East, eastConnection)
      (northConnection.roomCanMoveIndependently _) expects(*) returns(false) once()
      (northConnection.corridor _) expects() returns(northCorridor) anyNumberOfTimes()
      (southConnection.roomCanMoveIndependently _) expects(*) returns(false) once()
      (southConnection.corridor _) expects() returns(southCorridor) anyNumberOfTimes()
      (westConnection.roomCanMoveIndependently _) expects(*) returns(false) once()
      (westConnection.corridor _) expects() returns(westCorridor) anyNumberOfTimes()
      (eastConnection.roomCanMoveIndependently _) expects(*) returns(false) once()
      (eastConnection.corridor _) expects() returns(eastCorridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move in some direction")
      val areas = room.canMove(East)

      Then("one corridor for each direction should be returned")
      areas should have size (4)

      And("all corridors connected to the room should be returned")
      areas.toSet should be (Set(northCorridor, southCorridor, westCorridor, eastCorridor))

    }

    it ("should return northern adjacent areas not connected to the room when checking which areas must be moved") {

      Given("a room with a grid that returns an adjacent area north of the room")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(2, 0)
      val adjacentStop = Point(3, 2)
      val adjacentNorthArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(2, 2), Point(4, 2)) returns(Set(adjacentNorthArea)) once()

      When("checking which areas must be moved for the room to move north")
      val areas = room.canMove(North)

      Then("the adjacent area should be returned")
      areas should have size (1)
      areas.toSet should be (Set(adjacentNorthArea))

    }

    it ("should return southern adjacent areas not connected to the room when checking which areas must be moved") {

      Given("a room with a grid that returns an adjacent area south of the room")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(2, 4)
      val adjacentStop = Point(3, 6)
      val adjacentSouthArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(2, 4), Point(4, 4)) returns(Set(adjacentSouthArea)) once()

      When("checking which areas must be moved for the room to move south")
      val areas = room.canMove(South)

      Then("the adjacent area should be returned")
      areas should have size (1)
      areas.toSet should be (Set(adjacentSouthArea))

    }

    it ("should return western adjacent areas not connected to the room when checking which areas must be moved") {

      Given("a room with a grid that returns an adjacent area west of the room")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(0, 1)
      val adjacentStop = Point(2, 3)
      val adjacentWestArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(2, 2), Point(2, 4)) returns(Set(adjacentWestArea)) once()

      When("checking which areas must be moved for the room to move west")
      val areas = room.canMove(West)

      Then("the adjacent area should be returned")
      areas should have size (1)
      areas.toSet should be (Set(adjacentWestArea))

    }

    it ("should return eastern adjacent areas not connected to the room when checking which areas must be moved") {

      Given("a room with a grid that returns an adjacent area east of the room")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(4, 2)
      val adjacentStop = Point(5, 6)
      val adjacentEastArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(4, 2), Point(4, 4)) returns(Set(adjacentEastArea)) once()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("the adjacent area should be returned")
      areas should have size (1)
      areas.toSet should be (Set(adjacentEastArea))

    }

    it ("should return both connections and adjacent areas when moving in a direction") {

      Given("a room with a grid that returns an adjacent area south of the room, and a southern connection")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      // Adjacent area
      val adjacentStart = Point(2, 4)
      val adjacentStop = Point(3, 6)
      val adjacentSouthArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(2, 4), Point(4, 4)) returns(Set(adjacentSouthArea)) once()

      // Corridor connection
      val corridor = makeMockCorridor
      val connection = mock[RoomCorridorConnection]
      room.connect(South, connection)
      (connection.roomCanMoveIndependently _) expects(South) returns(false) once()
      (connection.corridor _) expects() returns(corridor) anyNumberOfTimes()
      (connection.room _) expects() returns(room) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move south")
      val areas = room.canMove(South)

      Then("the corridor connected to the room and the adjacent area should be returned")
      areas should have size (2)
      areas.toSet should be (Set(corridor, adjacentSouthArea))

    }

    it ("should not return areas in the grid that aren't adjacent to the room itself when checking which areas must be moved") {

      Given("a room with a grid that returns an area east of the room that isn't adjacent")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(5, 2)
      val adjacentStop = Point(6, 4)
      val adjacentEastArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(4, 2), Point(4, 4)) returns(Set(adjacentEastArea)) once()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("no areas should be returned")
      areas should be ('empty)

    }

    it ("should not return adjacent areas that only intersect a corner of the room") {

      Given("a room with a grid that returns an adjacent area east of the room that only intersects the lower right corner")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(4, 4)
      val adjacentStop = Point(6, 6)
      val adjacentEastArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(4, 2), Point(4, 4)) returns(Set(adjacentEastArea)) once()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("no areas should be returned")
      areas should be ('empty)

    }

    it ("should return an empty set when asked to move in a direction where its corridor connections doesn't need to be moved") {

      Given("a room with 4 connections that doesn't need to be moved")
      val room = makeRoomWithEmptyGrid(Point(3, 3), Point(5, 5))
      val northCorridor = makeMockCorridor
      val southCorridor = makeMockCorridor
      val westCorridor = makeMockCorridor
      val eastCorridor = makeMockCorridor
      val northConnection = mock[RoomCorridorConnection]
      val southConnection = mock[RoomCorridorConnection]
      val westConnection = mock[RoomCorridorConnection]
      val eastConnection = mock[RoomCorridorConnection]
      room.connect(North, northConnection)
      room.connect(South, southConnection)
      room.connect(West, westConnection)
      room.connect(East, eastConnection)
      (northConnection.roomCanMoveIndependently _) expects(*) returns(true) once()
      (northConnection.corridor _) expects() returns(northCorridor) anyNumberOfTimes()
      (southConnection.roomCanMoveIndependently _) expects(*) returns(true) once()
      (southConnection.corridor _) expects() returns(southCorridor) anyNumberOfTimes()
      (westConnection.roomCanMoveIndependently _) expects(*) returns(true) once()
      (westConnection.corridor _) expects() returns(westCorridor) anyNumberOfTimes()
      (eastConnection.roomCanMoveIndependently _) expects(*) returns(true) once()
      (eastConnection.corridor _) expects() returns(eastCorridor) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move in some direction")
      val areas = room.canMove(East)

      Then("no areas should be returned")
      areas should be ('empty)

    }

    it ("should not return areas that are already set to move in the direction of the room") {

      Given("a room with an eastern connection that doesn't allow the room to move independently and an non-connected adjacent area, both with set movement")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(4, 2)
      val adjacentStop = Point(6, 4)
      val adjacentEastArea = makeMockArea(adjacentStart, adjacentStop, East)
      (grid.elementsIn _) expects(Point(4, 2), Point(4, 4)) returns(Set(adjacentEastArea)) once()

      val eastCorridor = makeMockCorridor(East)
      val eastConnection = mock[RoomCorridorConnection]
      (eastConnection.corridor _) expects() returns(eastCorridor) anyNumberOfTimes()
      room.connect(East, eastConnection)

      (adjacentEastArea.movement _) expects() returns(Option(East)) anyNumberOfTimes()
      (eastCorridor.movement _) expects() returns(Option(East)) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("no areas should be returned")
      areas should be ('empty)

    }

    it ("should return areas that are neighbors in the direction the room is moving") {

      Given("a room that doesn't allow intersections, with a grid that returns an area east of the room that is a neighbor")
      val grid = mock[PartitionedArea[MutableArea]]
      val room = makeNonIntersectingRoomWithGrid(Point(2, 2), Point(4, 4), grid)

      val adjacentStart = Point(5, 2)
      val adjacentStop = Point(6, 4)
      val adjacentEastArea = makeMockArea(adjacentStart, adjacentStop)
      (grid.elementsIn _) expects(Point(5, 2), Point(5, 4)) returns(Set(adjacentEastArea)) once()
      (grid.start _) expects() returns(Point(2, 2)) anyNumberOfTimes()
      (grid.stop _) expects() returns(Point(8, 8)) anyNumberOfTimes()

      When("checking which areas must be moved for the room to move east")
      val areas = room.canMove(East)

      Then("the adjacent area should be returned")
      areas should have size (1)
      areas should contain (adjacentEastArea)

    }

  }

  private def makeDefaultRoom = new RoomArea(RectangularArea(Point(0, 0), Point(2, 2)), null, true)
  private def makeNonIntersectingRoomWithGrid(start: Point, stop: Point, grid: PartitionedArea[MutableArea])
  = new RoomArea(RectangularArea(start, stop), grid, false)

  private def makeRoomWithGrid(start: Point, stop: Point, grid: PartitionedArea[MutableArea])
  = new RoomArea(RectangularArea(start, stop), grid, true)

  private def makeRoomWithEmptyGrid(start: Point, stop: Point) = {
    val grid = mock[PartitionedArea[MutableArea]]
    (grid.elementsIn _) expects(*, *) returns(Set()) anyNumberOfTimes()

    makeRoomWithGrid(start, stop, grid)
  }

  private def makeMockArea(start: Point, stop: Point) = {
    val mockArea = mock[MutableArea]
    (mockArea.start _) expects() returns(start) anyNumberOfTimes()
    (mockArea.stop _) expects() returns(stop) anyNumberOfTimes()
    (mockArea.area _) expects() returns(RectangularArea(start, stop)) anyNumberOfTimes()
    (mockArea.movement _) expects() returns(None) anyNumberOfTimes()

    mockArea
  }

  private def makeMockArea(start: Point, stop: Point, movement: Direction): MutableArea = {
    val mockArea = mock[MutableArea]
    (mockArea.start _) expects() returns(start) anyNumberOfTimes()
    (mockArea.stop _) expects() returns(stop) anyNumberOfTimes()
    (mockArea.area _) expects() returns(RectangularArea(start, stop)) anyNumberOfTimes()
    (mockArea.movement _) expects() returns(Option(movement)) anyNumberOfTimes()

    mockArea
  }

  private def makeMockCorridor: MutableArea = {
    val corridor = mock[MutableArea]
    (corridor.movement _) expects() returns(None) anyNumberOfTimes()
    corridor
  }

  private def makeMockCorridor(direction: Direction): MutableArea = {
    val corridor = mock[MutableArea]
    (corridor.movement _) expects() returns(Option(direction)) anyNumberOfTimes()
    corridor
  }

}
