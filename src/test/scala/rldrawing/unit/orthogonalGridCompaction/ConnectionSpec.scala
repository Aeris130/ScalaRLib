package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{Connection, CorridorArea, RectangularArea, RoomArea}
import testHelpers.SpecImports

class ConnectionSpec extends SpecImports {

  /**
   * A corridor connecting to the west side of a room, with a start y-coordinate that is equal to the
   * rooms start y-coordinate.
   */
  def corridorAtEdgeOfRoomBoundary = new {
    val connectStart = 0
    val connectStop = 2
    val corridor = makeCorridor(Point(0, connectStart), Point(7, connectStop), West)

    val roomStart = 0
    val roomStop = 5
    val room = makeRoom(Point(7, roomStart), Point(10, roomStop))
  }

  /**
   * A square room with sides of length 9, and a corridor connecting to the south side in the middle.
   */
  def corridorAtCenterOfRoom = new {
    val roomStart = 0
    val roomStop = 8
    val room = makeRoom(Point(roomStart, roomStart), Point(roomStop, roomStop))

    val connectStart = 3
    val connectStop = 5
    val corridor = makeCorridor(Point(connectStart, 8), Point(connectStop, 10), South)
  }

  describe("Connection") {

    /*
     * Exceptions
     */

    it ("should throw an error if the deviation is negative") {

      Given("a negative deviation")
      val d = -1

      When("constructing a new connection")
      val f = corridorAtCenterOfRoom
      import f._

      Then("an error should be thrown")
      intercept[Error] {
        new Connection(room, corridor, South, d)
      }
    }

    it ("should throw an exception if the side of the corridor that connects to the room is larger than the rooms side") {

      Given("a room with a northern side of size 8 and a corridor that connects to it using its southern side of size 10")
      val roomStart = 10
      val roomStop = 17
      val room = makeRoom(Point(roomStart, roomStart), Point(roomStop, roomStop))

      val connectStart = 9
      val connectStop = 18
      val corridor = makeCorridor(Point(connectStart, 0), Point(connectStop, 17), South)

      When("constructing a new connection")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new Connection(room, corridor, North)
      }

    }

    it ("should throw an exception if the connected corridor fits inside the boundaries of the room, but diverges below its starting coordinate") {

      Given("a room with an east side with y-coordinates from 4 to 10 and a corridor that goes from 3 to 5")
      val roomStart = 4
      val roomStop = 10
      val room = makeRoom(Point(0, roomStart), Point(6, roomStop))

      val connectStart = 3
      val connectStop = 5
      val corridor = makeCorridor(Point(6, connectStart), Point(8, connectStop), West)

      When("constructing a new connection")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        new Connection(room, corridor, East)
      }

    }

    it ("should throw an exception if the connected corridor fits inside the boundaries of the room, but diverges above its stop coordinate") {

      Given("a room with an east side with y-coordinates from 4 to 10 and a corridor that goes from 8 to 11")
      val roomStart = 4
      val roomStop = 10
      val room = makeRoom(Point(0, roomStart), Point(6, roomStop))

      val connectStart = 8
      val connectStop = 11
      val corridor = makeCorridor(Point(6, connectStart), Point(8, connectStop), West)

      When("constructing a new connection")
      Then("an error should be thrown")
      intercept[IllegalArgumentException] {
        new Connection(room, corridor, East)
      }

    }

    it ("should throw an error if the room and corridor are disconnected (doesn't share coordinates on either axis)") {

      Given("a room and a corridor that doesn't share coordinates on any axis")
      val room = makeRoom(Point(0, 0), Point(6, 6))
      val corridor = makeCorridor(Point(8, 8), Point(10, 10), West)

      When("constructing a new connection")
      Then("an error should be thrown")
      intercept[Error] {
        new Connection(room, corridor, East)
      }

    }

    it ("should throw an error if the room and corridor overlap more than a single coordinate parallel to the connection axis") {

      Given("a room and a corridor that overlaps the room inside its borders")
      val room = makeRoom(Point(0, 0), Point(6, 6))
      val corridor = makeCorridor(Point(5, 2), Point(8, 5), West)

      When("constructing a new connection")
      Then("an error should be thrown")
      intercept[Error] {
        new Connection(room, corridor, East)
      }

    }


    /*
     * Corridor movement parallel to its connection
     */

    it ("should allow corridor movement when there is coordinates left within the boundary in the direction the corridor is moving") {

      Given("a corridor that connects to a room on its east side at the top of the sides axis, with free coordinates below the corridor")
      val f = corridorAtEdgeOfRoomBoundary
      import f._

      val connection = new Connection(room, corridor, West)

      When("checking if the corridor can be moved downward 1 coordinate on the y axis")
      val result = connection.corridorCanMoveIndependently(South)

      Then("the result should be true")
      result should be (true)

    }

    it ("should not allow corridor movement when the corridor is at the edge of the boundary in the direction it is moving") {

      Given("a corridor that connects to a room on its west side at the top of the sides axis, with no free coordinates to the north")
      val f = corridorAtEdgeOfRoomBoundary
      import f._

      val connection = new Connection(room, corridor, West)

      When("checking if the corridor can be moved upward 1 coordinate on the y axis")
      val result = connection.corridorCanMoveIndependently(North)

      Then("the result should be false")
      result should be (false)

    }

    it ("should allow corridor movement when the corridor is within the room area boundary as well as the deviation") {

      Given("a room with more than 1 coordinate to move in either direction of the room boundary, and a max deviation of 1")
      val f = corridorAtCenterOfRoom
      import f._

      val deviation = 1
      val connection = new Connection(room, corridor, South, deviation)

      When("checking if the corridor can be moved 1 coordinate in either direction of the boundary")
      val canMoveWest = connection.corridorCanMoveIndependently(West)
      val canMoveEast = connection.corridorCanMoveIndependently(East)

      Then("the result should be true")
      canMoveWest should be (true)
      canMoveEast should be (true)

    }

    it ("should not allow corridor movement when the corridor is within the room area boundary but without deviation left") {

      Given("a room with more than 1 coordinate to move in either direction of the room boundary, and a max deviation of 0")
      val f = corridorAtCenterOfRoom
      import f._

      val deviation = 0
      val connection = new Connection(room, corridor, South, deviation)

      When("checking if the corridor can be moved 1 coordinate in either direction of the boundary")
      val canMoveWest = connection.corridorCanMoveIndependently(West)
      val canMoveEast = connection.corridorCanMoveIndependently(East)

      Then("the result should be false")
      canMoveWest should be (false)
      canMoveEast should be (false)

    }

    /*
     * Room movement parallel to its connection
     */

    it ("should allow room movement when there is coordinates left within the boundary in the direction the room is moving") {

      Given("a corridor that connects to a room on its east side at the top of the sides axis, with free coordinates below the corridor")
      val f = corridorAtEdgeOfRoomBoundary
      import f._

      val connection = new Connection(room, corridor, West)

      When("checking if the room can be moved upward 1 coordinate on the y axis")
      val result = connection.roomCanMoveIndependently(North)

      Then("the result should be true")
      result should be (true)

    }

    it ("should not allow room movement when its corridor is at the edge of the boundary in the opposite direction it is moving") {

      Given("a corridor that connects to a room on its west side at the top of the sides axis, with no free coordinates to the north")
      val f = corridorAtEdgeOfRoomBoundary
      import f._

      val connection = new Connection(room, corridor, West)

      When("checking if the room can be moved downward 1 coordinate on the y axis")
      val result = connection.roomCanMoveIndependently(South)

      Then("the result should be false")
      result should be (false)

    }

    it ("should allow room movement when the corridor is within the room area boundary as well as the deviation") {

      Given("a room with more than 1 coordinate to move in either direction of the room boundary, and a max deviation of 1")
      val f = corridorAtCenterOfRoom
      import f._

      val deviation = 1
      val connection = new Connection(room, corridor, South, deviation)

      When("checking if the room can be moved 1 coordinate in either direction of the boundary")
      val canMoveWest = connection.roomCanMoveIndependently(West)
      val canMoveEast = connection.roomCanMoveIndependently(East)

      Then("the result should be true")
      canMoveWest should be (true)
      canMoveEast should be (true)

    }

    it ("should not allow room movement when the corridor is within the room area boundary but without deviation left") {

      Given("a room with more than 1 coordinate to move in either direction of the room boundary, and a max deviation of 0")
      val f = corridorAtCenterOfRoom
      import f._

      val deviation = 0
      val connection = new Connection(room, corridor, South, deviation)

      When("checking if the room can be moved 1 coordinate in either direction of the boundary")
      val canMoveWest = connection.roomCanMoveIndependently(West)
      val canMoveEast = connection.roomCanMoveIndependently(East)

      Then("the result should be false")
      canMoveWest should be (false)
      canMoveEast should be (false)

    }

    it ("should compute correct boundaries for a room that has adjusted its coordinates once") {

      Given("a 3x3 sized room connected to a 3x3 sized south corridor")
      val roomStart = 1
      val roomStop = 3
      val room = makeRoom(Point(roomStart, roomStart), Point(roomStop, roomStop))
      val corridor = makeCorridor(Point(1, 3), Point(3, 5), North)

      val connection = new Connection(room, corridor, South, 0)

      When("checking if the room can be moved west after adjusting the room and corridor position")
      room.setNewArea(room.area.adjustCoordinates(West, 1))
      corridor.setNewArea(corridor.area.adjustCoordinates(West, 1))
      connection.roomCanMoveIndependently(West) should be (false)
      connection.corridorCanMoveIndependently(West) should be (false)

    }

    /*
     * Moving rooms and corridors in the direction they're connected.
     */

    it ("should not allow a corridor to move in the direction of its connected room") {

      Given("a corridor that is connected to the west side of a room")
      val f = corridorAtEdgeOfRoomBoundary
      import f._
      val connection = new Connection(room, corridor, West)

      When("checking if the corridor can move east")
      val result = connection.corridorCanMoveIndependently(East)

      Then("the result should be false")
      result should be (false)

    }

    it ("should not allow the room to move in the direction of its connected corridor") {

      Given("a room and a corridor that connects to the south side of it")
      val f = corridorAtCenterOfRoom
      import f._
      val connection = new Connection(room, corridor, South)

      When("checking if the room can move south")
      val result = connection.roomCanMoveIndependently(South)

      Then("the result should be false")
      result should be (false)
    }

    it ("should not allow a corridor to move away from its connected room") {

      Given("a corridor that is connected to the west side of a room")
      val f = corridorAtEdgeOfRoomBoundary
      import f._
      val connection = new Connection(room, corridor, West)

      When("checking if the corridor can move west")
      val result = connection.corridorCanMoveIndependently(West)

      Then("the result should be false")
      result should be (false)

    }

    it ("should not allow a room to move away from its connected corridor") {

      Given("a room and a corridor that connects to the south side of it")
      val f = corridorAtCenterOfRoom
      import f._
      val connection = new Connection(room, corridor, South)

      When("checking if the room can move north")
      val result = connection.roomCanMoveIndependently(North)

      Then("the result should be false")
      result should be (false)

    }

  }

  private def makeRoom(start: Point, stop: Point): RoomArea = new RoomArea(RectangularArea(start, stop), null, true)
  private def makeCorridor(start: Point, stop: Point, dir: Direction) = new CorridorArea(RectangularArea(start, stop), null, dir, true)
}
