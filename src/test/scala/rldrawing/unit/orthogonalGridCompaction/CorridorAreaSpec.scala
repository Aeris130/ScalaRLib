package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{AdjustableRectangle, _}
import testHelpers.SpecImports

class CorridorAreaSpec extends SpecImports {

  describe("CorridorArea") {

    it ("should specify area type") {

      Given("a corridor")
      val corridor =  defaultCorridor(North)

      When("checking area type")
      Then("the type should be corridor")
      corridor.isRoom should be (false)
      corridor.isBend should be (false)
      corridor.isCorridor should be (true)

    }

    it ("should throw an error when attempting to connect two rooms that aren't opposite of north") {

      Given("a corridor that holds connections to the north/south")
      val corridor = defaultCorridor(North)

      When("connecting an area to the north and west")
      val northArea = mock[RoomCorridorConnection]
      corridor.connect(North, northArea)

      Then("an exception should be thrown")
      val westArea = mock[RoomCorridorConnection]
      intercept[Error] {
        corridor.connect(West, westArea)
      }
    }

    it ("should throw an error when attempting to connect two rooms that aren't opposite of south") {

      Given("a corridor that holds connections to the north/south")
      val corridor = defaultCorridor(North)

      When("connecting an area to the south and west")
      val southArea = mock[RoomCorridorConnection]
      corridor.connect(South, southArea)

      Then("an exception should be thrown")
      val westArea = mock[RoomCorridorConnection]
      intercept[Error] {
        corridor.connect(West, westArea)
      }
    }

    it ("should throw an error when attempting to connect two rooms that aren't opposite of west") {

      Given("a corridor that holds connections to the west/east")
      val corridor = defaultCorridor(West)

      When("connecting an area to the west and north")
      val westArea = mock[RoomCorridorConnection]
      corridor.connect(West, westArea)

      Then("an exception should be thrown")
      val northArea = mock[RoomCorridorConnection]
      intercept[Error] {
        corridor.connect(North, northArea)
      }
    }

    it ("should throw an error when attempting to connect two rooms that aren't opposite of east") {

      Given("a corridor that holds connections to the west/east")
      val corridor = defaultCorridor(West)

      When("connecting an area to the east and south")
      val eastArea = mock[RoomCorridorConnection]
      corridor.connect(East, eastArea)

      Then("an exception should be thrown")
      val southArea = mock[RoomCorridorConnection]
      intercept[Error] {
        corridor.connect(South, southArea)
      }
    }

    it ("should throw an error if the corridor is created with less than size 3 in the north/south direction it connects") {

      Given("a rectangle with width 2")
      val start = Point(0, 0)
      val stop = Point(1, 3)

      When("creating a corridor that connects north and south")
      Then("an error should be thrown")
      intercept[Error] {
        makeCorridor(start, stop, null, North)
      }
      intercept[Error] {
        makeCorridor(start, stop, null, South)
      }

    }

    it ("should throw an error if the corridor is created with less than size 3 in the west/east direction it connects") {

      Given("a rectangle with width 2")
      val start = Point(0, 0)
      val stop = Point(4, 1)

      When("creating a corridor that connects west and east")
      Then("an error should be thrown")
      intercept[Error] {
        makeCorridor(start, stop, null, West)
      }
      intercept[Error] {
        makeCorridor(start, stop, null, East)
      }

    }

    /*
     * Movement
     */
    it ("should throw an error if attempting to move without setting a movement direction") {

      Given("a corridor without movement set")
      val c = defaultCorridor(North)

      When("attempting to move")
      Then("an error should be thrown")
      intercept[Error] {
        c.move
      }

    }

    it ("should move in a direction that has no connections on either side") {

      Given("a corridor with no connections to the north/south")
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East) // length 4, width 3

      When("moving the corridor north")
      corridor.markAsMoved(North)
      corridor.move

      Then("the corridor should have its coordinates shifted north by 1")
      corridor.area should be (AdjustableRectangle(Point(3, 0), Point(6, 2)))

    }

    it ("should move in the direction of one of its connections if both connections are set to move") {

      Given("a corridor connected to two rooms (west/east) that both move in the direction of the corridor")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East) // length 4, width 3
      val eastRoom = makeRoomWithEmptyGrid(Point(6, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      eastRoom.markAsMoved(East)
      corridor.markAsMoved(East)

      When("moving the corridor east")
      corridor.move

      Then("the corridor should retain its length")
      corridor.area.lengthOfSide(North) should be (4)

      And("its coordinates should be adjusted east by 1")
      corridor.area should be (AdjustableRectangle(Point(4, 1), Point(7, 3)))

    }

    it ("should shrink its length when moving in the direction of one of its connections and only the other connection moves") {

      Given("a corridor with no minimum length that moves east towards a room that doesn't move")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East) // length 4, width 3
      val eastRoom = makeRoomWithEmptyGrid(Point(6, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      // East room doesn't move
      corridor.markAsMoved(East)

      When("moving the corridor east")
      corridor.move

      Then("the corridor should have length 3 instead of 4")
      corridor.area.lengthOfSide(North) should be (3)

      And("its two eastern points should remain as is")
      corridor.area.coordinatesForSide(East) should be (Point(6, 1), Point(6, 3))

      And("its two western points should be adjusted east by 1")
      corridor.area.coordinatesForSide(West) should be (Point(4, 1), Point(4, 3))

    }

    it ("should throw an exception if one of its connections are moving away from the other connection and it doesn't move as well") {

      Given("a corridor connected to a west and an east room that move in different directions")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East) // length 4, width 3
      val eastRoom = makeRoomWithEmptyGrid(Point(6, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      eastRoom.markAsMoved(North) // Different
      corridor.markAsMoved(East)

      When("moving the corridor east")
      Then("an exception should be thrown")
      intercept[Error] {
        corridor.move
      }

    }

    it ("should throw an exception when moving towards one of its connections that doesn't move and its length is 1") {

      Given("two rooms connected by a corridor of length 1, and the eastern room missing movement direction")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(3, 3), East) // length 1
      val eastRoom = makeRoomWithEmptyGrid(Point(3, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      corridor.markAsMoved(East)

      When("moving the corridor east")
      Then("an error should be thrown")
      intercept[Error] {
        corridor.move
      }

    }

    it ("should thrown an exception when moving if only one connection is set") {

      Given("a corridor with a single connection")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(3, 3), West)
      connect(westRoom, corridor, East)
      corridor.markAsMoved(East)

      When("moving the corridor east")
      Then("an error should be thrown")
      intercept[Error] {
        corridor.move
      }

    }

    /*
     * Checking adjacent areas to move
     */

    it ("should not return itself") {

      Given("a corridor, a room and a grid that returns both as adjacent to the corridor")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, cStop, grid, East)
      val northArea = makeMockArea(Point(1, 1), Point(7, 6))
      (grid.elementsIn _) expects(*, *) returns(Set(northArea, corridor)) once()

      When("which areas obstruct the corridor from moving towards the north area")
      val obstructingAreas = corridor.canMove(North)

      Then("only the northern area should be returned")
      obstructingAreas should be (Set(northArea))

    }

    it ("should return adjacent non-connected areas north of it when moving in that direction") {

      Given("a corridor with a grid that returns an adjacent area north of it")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, Point(8, 9), grid, East)
      val northArea = makeMockArea(Point(1, 1), Point(7, 6))
      (grid.elementsIn _) expects(cStart, Point(cStop.x, cStart.y)) returns(Set(northArea)) once()

      When("checking for adjacent areas to the north")
      val adjacentAreas = corridor.canMove(North)

      Then("the adjacent area should be returned")
      adjacentAreas should be (Set(northArea))

    }

    it ("should return adjacent non-connected areas south of it when moving in that direction") {

      Given("a corridor with a grid that returns an adjacent area south of it")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, Point(8, 9), grid, East)
      val southArea = makeMockArea(Point(5, 9), Point(7, 13))
      (grid.elementsIn _) expects(Point(cStart.x, cStop.y), cStop) returns(Set(southArea)) once()

      When("checking for adjacent areas to the south")
      val adjacentAreas = corridor.canMove(South)

      Then("the adjacent area should be returned")
      adjacentAreas should be (Set(southArea))

    }

    it ("should return adjacent non-connected areas west of it when moving in that direction") {

      Given("a corridor with a grid that returns an adjacent area west of it")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, Point(8, 9), grid, East)
      val westArea = makeMockArea(Point(1, 1), Point(5, 7))
      (grid.elementsIn _) expects(cStart, Point(cStart.x, cStop.y)) returns(Set(westArea)) once()

      When("checking for adjacent areas to the west")
      val adjacentAreas = corridor.canMove(West)

      Then("the adjacent area should be returned")
      adjacentAreas should be (Set(westArea))

    }

    it ("should return adjacent non-connected areas east of it when moving in that direction") {

      Given("a corridor with a grid that returns an adjacent area east of it")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, Point(8, 9), grid, East)
      val eastArea = makeMockArea(Point(8, 7), Point(13, 10))
      (grid.elementsIn _) expects(Point(cStop.x, cStart.y), cStop) returns(Set(eastArea)) once()

      When("checking for adjacent areas to the east")
      val adjacentAreas = corridor.canMove(East)

      Then("the adjacent area should be returned")
      adjacentAreas should be (Set(eastArea))

    }

    it ("shouldn't return any areas when moving in parallel with its connections that allows it") {

      Given("a corridor with two connections that allows the corridor to slide on the border of their rooms")
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(3, 3), East)
      val connectionEast = mock[RoomCorridorConnection]
      val connectionWest = mock[RoomCorridorConnection]
      corridor.connect(West, connectionWest)
      corridor.connect(East, connectionEast)

      (connectionEast.corridorCanMoveIndependently _) expects(North) returns(true) anyNumberOfTimes()
      (connectionWest.corridorCanMoveIndependently _) expects(North) returns(true) anyNumberOfTimes()

      val eastRoom = makeMockArea(Point(3, 1), Point(5, 3))
      val westRoom = makeMockArea(Point(1, 1), Point(3, 3))

      (connectionEast.room _) expects() returns(eastRoom) anyNumberOfTimes()
      (connectionWest.room _) expects() returns(westRoom) anyNumberOfTimes()

      When("checking if the corridor can move north unhindered")
      val adjacentAreas = corridor.canMove(North)

      Then("no areas should be returned")
      adjacentAreas should be ('empty)

    }

    it ("should return a room connected behind the direction it is moving") {

      Given("a corridor with connections to the east/west")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(3, 3), East)
      val eastRoom = makeRoomWithEmptyGrid(Point(3, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)

      When("checking for areas required to move east")
      val adjacentAreas = corridor.canMove(East)

      Then("the room connected to the west should be returned")
      assert(adjacentAreas contains westRoom, "No room west of the corridor found in the set " + adjacentAreas)

    }

    it ("should not return a room connected in the direction it is moving if the room behind it already has registered movement and length > minimum") {

      Given("a corridor with length > 1 and min length = 1, connected to a west and east room, with the west room having eastern movement registered")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East)
      val eastRoom = makeRoomWithEmptyGrid(Point(6, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      corridor.markAsMoved(East)

      When("checking for areas required to move east")
      val adjacentAreas = corridor.canMove(East)

      Then("the eastern room should not be returned")
      adjacentAreas should not contain (eastRoom)

    }

    it ("should return a room connected in the direction it is moving if the room behind it already has registered movement and length == minimum") {

      Given("a corridor with length = 1 and min length = 1, connected to a west and east room, with only the west room having eastern movement registered")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val minLength = 1
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(3, 3), East, minLength)
      val eastRoom = makeRoomWithEmptyGrid(Point(3, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      westRoom.markAsMoved(East)
      corridor.markAsMoved(East)

      When("checking for areas required to move east")
      val adjacentAreas = corridor.canMove(East)

      Then("the eastern room should be returned")
      assert(adjacentAreas contains eastRoom, "No room east of the corridor found in the set " + adjacentAreas)

    }

    it ("should return the room behind it if it has no movement and the room ahead does") {

      Given("a corridor above min length, connected to an eastern and western room, with the western room having no movement")
      val westRoom = makeRoomWithEmptyGrid(Point(1, 1), Point(3, 3))
      val corridor = makeCorridorWithEmptyGrid(Point(3, 1), Point(6, 3), East)
      val eastRoom = makeRoomWithEmptyGrid(Point(6, 1), Point(8, 3))
      connect(westRoom, corridor, East)
      connect(eastRoom, corridor, West)
      eastRoom.markAsMoved(East)
      corridor.markAsMoved(East)

      When("checking for areas required to move east")
      val adjacentAreas = corridor.canMove(East)

      Then("both rooms should be returned")
      adjacentAreas should be (Set(westRoom))

    }

    it ("should not return connections or adjacent areas that already has its movement set") {

      Given("a corridor with two connections and a grid that returns an adjacent area south of it, and all areas having movement set")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeCorridor(cStart, Point(8, 9), grid, East)
      val southArea = makeMockArea(Point(5, 9), Point(7, 13), South)
      (grid.elementsIn _) expects(Point(cStart.x, cStop.y), cStop) returns(Set(southArea)) once()

      val connectionEast = mock[RoomCorridorConnection]
      val connectionWest = mock[RoomCorridorConnection]
      corridor.connect(West, connectionWest)
      corridor.connect(East, connectionEast)

      val eastRoom = mock[MutableArea]
      val westRoom = mock[MutableArea]

      (connectionEast.corridorCanMoveIndependently _) expects(South) returns(false) anyNumberOfTimes()
      (connectionWest.corridorCanMoveIndependently _) expects(South) returns(false) anyNumberOfTimes()
      (connectionEast.room _) expects() returns(eastRoom) anyNumberOfTimes()
      (connectionWest.room _) expects() returns(westRoom) anyNumberOfTimes()

      (eastRoom.movement _) expects() returns(Option(South)) anyNumberOfTimes()
      (westRoom.movement _) expects() returns(Option(South)) anyNumberOfTimes()

      When("checking for adjacent areas to the south")
      val adjacentAreas = corridor.canMove(South)

      Then("No areas should be returned")
      adjacentAreas should be ('empty)

    }

    it ("should return areas that are non-connecting neighbors in the direction the room is moving") {

      Given("a corridor that doesn't allow intersections, with a grid that returns an neighboring area east of it")
      val grid = mock[PartitionedArea[MutableArea]]
      val cStart = Point(5, 6)
      val cStop = Point(8, 9)
      val corridor = makeNonIntersectingCorridor(cStart, cStop, grid, East)
      val eastArea = makeMockArea(Point(9, 7), Point(13, 10))
      (grid.elementsIn _) expects(Point(cStop.x + 1, cStart.y), Point(cStop.x + 1, cStop.y)) returns(Set(eastArea)) once()
      (grid.start _) expects() returns(Point(0, 0)) anyNumberOfTimes()
      (grid.stop _) expects() returns(Point(15, 15)) anyNumberOfTimes()

      When("checking for adjacent areas to the east")
      val adjacentAreas = corridor.canMove(East)

      Then("the adjacent area should be returned")
      adjacentAreas should be (Set(eastArea))

    }

  }

  private def connect(room: RoomArea, corridor: CorridorArea, dir: Direction): Unit = {
    val connection = new Connection(room, corridor, dir)
    room.connect(dir, connection)
    corridor.connect(dir.opposite, connection)
  }

  private def makeCorridor(start: Point, stop: Point, grid: PartitionedArea[MutableArea], dir: Direction) = new CorridorArea(AdjustableRectangle(start, stop), grid, dir, true)
  private def makeNonIntersectingCorridor(start: Point, stop: Point, grid: PartitionedArea[MutableArea], dir: Direction) = new CorridorArea(AdjustableRectangle(start, stop), grid, dir, false)
  private def makeCorridor(start: Point, stop: Point, grid: PartitionedArea[MutableArea], dir: Direction, minLength: Int) =
    new CorridorArea(AdjustableRectangle(start, stop), grid, dir, minLength, true)

  private def makeCorridorWithEmptyGrid(start: Point, stop: Point, dir: Direction) = {
    val grid = mock[PartitionedArea[MutableArea]]
    (grid.elementsIn _) expects(*, *) returns(Set()) anyNumberOfTimes()

    makeCorridor(start, stop, grid, dir)
  }

  private def makeCorridorWithEmptyGrid(start: Point, stop: Point, dir: Direction, minLength: Int) = {
    val grid = mock[PartitionedArea[MutableArea]]
    (grid.elementsIn _) expects(*, *) returns(Set()) anyNumberOfTimes()

    makeCorridor(start, stop, grid, dir, minLength)
  }

  private def defaultCorridor(dir: Direction) = new CorridorArea(AdjustableRectangle(Point(3, 3), Point(5, 6)), null, dir, true)

  private def makeRoomWithGrid(start: Point, stop: Point, grid: PartitionedArea[MutableArea])
  = new RoomArea(AdjustableRectangle(start, stop), grid, true)

  private def makeRoomWithEmptyGrid(start: Point, stop: Point) = {
    val grid = mock[PartitionedArea[MutableArea]]
    (grid.elementsIn _) expects(*, *) returns(Set()) anyNumberOfTimes()

    makeRoomWithGrid(start, stop, grid)
  }


  private def makeMockArea(start: Point, stop: Point): MutableArea = {
    val mockArea = mock[MutableArea]
    (mockArea.start _) expects() returns(start) anyNumberOfTimes()
    (mockArea.stop _) expects() returns(stop) anyNumberOfTimes()
    (mockArea.area _) expects() returns(AdjustableRectangle(start, stop)) anyNumberOfTimes()
    (mockArea.movement _) expects() returns(None) anyNumberOfTimes()

    mockArea
  }

  private def makeMockArea(start: Point, stop: Point, movement: Direction): MutableArea = {
    val mockArea = mock[MutableArea]
    (mockArea.start _) expects() returns(start) anyNumberOfTimes()
    (mockArea.stop _) expects() returns(stop) anyNumberOfTimes()
    (mockArea.area _) expects() returns(AdjustableRectangle(start, stop)) anyNumberOfTimes()
    (mockArea.movement _) expects() returns(Option(movement)) anyNumberOfTimes()

    mockArea
  }

}
