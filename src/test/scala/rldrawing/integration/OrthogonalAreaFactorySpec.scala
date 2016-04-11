package rldrawing.integration

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.drawing.{DrawnEdge, OrthogonalLayout}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{AdjustableRectangle, _}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.{GridPartitionFactory, OrthogonalAreaFactory}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.util.GridPartition
import rldrawing.help.{ConstraintEdge, ConstraintRoom}
import testHelpers.SpecImports

class OrthogonalAreaFactorySpec extends SpecImports {
  private val intersectingAreaFactory = new OrthogonalAreaFactory(true)
  private val nonIntersectingFactory = new OrthogonalAreaFactory(false)

  def customFactory(fact: PartitionedArea[MutableArea]): GridPartitionFactory[MutableArea] = {
    class Fact extends GridPartitionFactory[MutableArea] {
      def createGrid(start: Point, stop: Point, roomAmount: Int): PartitionedArea[MutableArea] = fact
    }

    new Fact()
  }

  /**
   * A 5x5 room at coordinate 0,1 with a bending corridor to a 3x3 room at 1,0.
   */
  def twoRoomsWithBend = new {
    val room1 = new ConstraintRoom(5, 5)
    val room2 = new ConstraintRoom(3, 3)
    val r1X = 1
    val r1Y = 0
    val r2X = 0
    val r2Y = 1
    val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

    val minLength = 2
    val edge = ConstraintEdge(room1, room2, 3, minLength, 0)
    val bends = Vector((0, 0))
    val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

    val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

    val room1Start = Point(5, 0)
    val room1Stop = Point(9, 4)
    val room2Start = Point(1, 6)
    val room2Stop = Point(3, 8)
  }

  /**
   * Two 3x3 rooms at (0,0) and (0,1) with bends going (0,0) -> b(1,0) -> b(1,1) -> (0,1)
   */
  def twoRoomsWithTwoBends = new {
    val room1 = new ConstraintRoom(3, 3)
    val room2 = new ConstraintRoom(3, 3)
    val r1X = 0
    val r1Y = 0
    val r2X = 0
    val r2Y = 1
    val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

    val edge = ConstraintEdge(room1, room2, 3)
    val bends = Vector((1, 0), (1, 1))
    val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

    val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

    val room1Start = Point(0, 0)
    val room1Stop = Point(2, 2)
    val room2Start = Point(0, 3)
    val room2Stop = Point(2, 5)
  }

  describe("OrthogonalAreaFactory") {

    it ("should produce a single room area") {

      Given("an orthogonal drawing with a single room at coordinate 0 with width 5 and height 5")
      val room = new ConstraintRoom(5, 5)
      val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room, 0, 0)))

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the result should contain a single room with area from (0,0) to (4,4)")
      areaRepresentation.rooms should have size (1)
      areaRepresentation.rooms.head.area.start should be (Point(0, 0))
      areaRepresentation.rooms.head.area.stop should be (Point(4, 4))

    }

    it ("should align rooms closer to (0,0) when a large room creates grid sizes that can't evenly fit all rooms") {

      Given("a drawing with a room at coordinate (0,0) with sides of length 3, and a room at (1,0) with height 4 and width 5")
      val room1 = new ConstraintRoom(3, 3)
      val room2 = new ConstraintRoom(5, 4)
      val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room1, 0, 0), (room2, 1, 0)))

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the second room should start at coordinate 5 on the x axis and end 5 coordinates down the x axis (9) and 4 coordinates down the y axis (3)")
      areaRepresentation.rooms should have size (2)
      assert(areaRepresentation.rooms.exists(r => r.start == Point(5, 0) && r.stop == Point(9, 3)))

    }

    it ("should produce a corridor segment with no bends between two rooms, with the same width as the room sides and length 2") {

      Given("two rooms with sides of length 5, and a corridor with width 5")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(5, 5)
      val roomList = Vector((room1, 0, 0), (room2, 1, 0))
      val edge = ConstraintEdge(room1, room2, 5)
      val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 0), Vector(), edge)
      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("there should be a corridor starting in room 1 and ending in room 2 with no bends and width 5")
      val room1Area = AdjustableRectangle(Point(0, 0), Point(4, 4))
      val room2Area = AdjustableRectangle(Point(5, 0), Point(9, 4))
      val corridor = areaRepresentation.corridors.find(c => c.from.area == room1Area && c.to.area == room2Area).getOrElse {
        fail("No rooms with areas " + room1Area + " and " + room2Area + " found in corridor set " + areaRepresentation.corridors)
      }

      corridor.areas should have size (1)
      corridor.areas.head.area should be (AdjustableRectangle(Point(4, 0), Point(5, 4)))

    }

    it ("should connect multiple corridor segments using bends") {

      Given("a room with sides of length 5, and a room to the south-west of it with height 5 and width 3 having an edge of width 3 between them")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(3, 5)
      val r1X = 1
      val r1Y = 0
      val r2X = 0
      val r2Y = 2
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val edge = ConstraintEdge(room1, room2, 3)
      val bends = Vector((0, 0))
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("there should be a corridor from room 1 to a 3x3 bend at (0,0)")
      val room1Area = AdjustableRectangle(Point(5, 0), Point(9, 4)) // 5x5
      val room2Area = AdjustableRectangle(Point(1, 10), Point(3, 14)) // 3x5 area in a 5x5 grid, so it gets adjusted 1 step inward
      val corridor = areaRepresentation.corridors.find(c => c.from.area == room1Area && c.to.area == room2Area).getOrElse {
          fail("No rooms with areas " + room1Area + " and " + room2Area + " found in corridor set " + areaRepresentation.corridors)
        }

      And("the first segment should be a corridor between area 1 and a 3x3 bend at (1,1)~(3,3)")
      val firstSegment = corridor.areas(0)
      firstSegment.isCorridor should be (true)
      firstSegment.start should be (Point(3, 1))
      firstSegment.stop should be (Point(5, 3))

      And("the second segment should be a 3x3 bend")
      val secondSegment = corridor.areas(1)
      secondSegment.isBend should be (true)
      secondSegment.start should be (Point(1, 1))
      secondSegment.stop should be (Point(3, 3))

      And("the third segment should be a corridor between the bend and area 2")
      val thirdSegment = corridor.areas(2)
      thirdSegment.isCorridor should be (true)
      thirdSegment.start should be (Point(1, 3))
      thirdSegment.stop should be (Point(3, 10))

    }

    it ("should align corridors closer to (0, 0) when the room boundary cannot split the corridor evenly") {

      Given("two rooms with sides 6 and a corridor of width 3")
      val room1 = new ConstraintRoom(6, 6)
      val room2 = new ConstraintRoom(6, 6)
      val roomList = Vector((room1, 0, 1), (room2, 0, 0)) // Room 1 above room 2

      val edge = ConstraintEdge(room1, room2, 3)
      val drawnEdge = new DrawnEdge(room1, room2, (0, 1), (0, 0), Vector(), edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the corridor should be aligned to the left part of the room boundary (1 to 3 on the x axis instead of 2 to 4)")
      val corridor = areaRepresentation.corridors.head
      val singleSegment = corridor.areas.head
      singleSegment.start should be (Point(1, 5))
      singleSegment.stop should be (Point(3, 6))

    }

    it ("should set the minimum length of a corridor segment to the largest value possible if the enclosing areas does not " +
      "allow a length equal or higher than the user-submitted minimum length") {

      Given("two rooms of equal size (meaning they will leave no empty coordinates between them) and a corridor with min length 5")
      val room1 = new ConstraintRoom(3, 3)
      val room2 = new ConstraintRoom(3, 3)
      val roomList = Vector((room1, 0, 0), (room2, 1, 0)) // Room 1 above room 2

      val minLength = 5
      val edge = ConstraintEdge(room1, room2, 3, minLength, 0)
      val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 0), Vector(), edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the resulting corridor segment should have minimum length 2 (from 2 to 3 on the x axis)")
      val corridorCast = areaRepresentation.corridors.head.areas.head.asInstanceOf[CorridorArea]
      corridorCast.minLength should be (2)

    }

    it ("should set the minimum length of a every corridor segment in a sequence of bends to the individual length of" +
      "that segment if the length is less than the user-specified minimum length") {

      Given("two rooms separated by a bend such that each edge segment is shorter than the minimum length, and differ in length")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(3, 3)
      val r1X = 1
      val r1Y = 0
      val r2X = 0
      val r2Y = 1
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val minLength = 5
      val edge = ConstraintEdge(room1, room2, 3, minLength, 0)
      val bends = Vector((0, 0))
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)
      val corridor = areaRepresentation.corridors.head

      Then("the min length of the edge segment between room 1 and the bend should be 3")
      val firstSegment = corridor.areas(0)
      firstSegment.asInstanceOf[CorridorArea].minLength should be (3)

      And("the min length of the edge segment between the bend and room 2 should be 4")
      val thirdSegment = corridor.areas(2)
      thirdSegment.asInstanceOf[CorridorArea].minLength should be (4)

    }

    it ("should set the user-specified minimum length when a single corridor segment is longer than the minimum length") {

      Given("two rooms connected by a corridor of length 4 and an edge with minimum length 2")
      val room1 = new ConstraintRoom(3, 3)
      val room2 = new ConstraintRoom(3, 5)
      val roomList = Vector((room1, 0, 0), (room2, 1, 0)) // Room 1 above room 2

      val minLength = 2
      val edge = ConstraintEdge(room1, room2, 3, minLength, 0)
      val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 0), Vector(), edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the resulting corridor segment should have minimum length 2")
      val corridorCast = areaRepresentation.corridors.head.areas.head.asInstanceOf[CorridorArea]
      corridorCast.minLength should be (2)

    }

    it ("should set every segments minimum length to the user-specified value when a bending corridors segments are higher than the minimum value") {

      Given("two rooms separated by a bend such that the first segment is of length 3, and the second 4, with minimum length 2")
      val f = twoRoomsWithBend
      import f._

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)
      val corridor = areaRepresentation.corridors.head

      Then("the min length of the edge segment between room 1 and the bend should be 2")
      val firstSegment = corridor.areas(0)
      firstSegment.asInstanceOf[CorridorArea].minLength should be (2)

      And("the min length of the edge segment between the bend and room 2 should be 2")
      val thirdSegment = corridor.areas(2)
      thirdSegment.asInstanceOf[CorridorArea].minLength should be (2)

    }

    it ("should add every room, corridor and bend to a partitioned grid") {

      Given("two rooms with a corridor consisting of one bend and two corridor segments")
      val f = twoRoomsWithBend
      import f._

      When("generating areas from the drawing using a custom grid")
      val gridStart = Point(0, 0)
      val gridStop = Point(9, 9)
      val grid = new GridPartition[MutableArea](gridStart, gridStop)
      val fact = customFactory(grid)
      val areaFactory = new OrthogonalAreaFactory(fact, true)
      val areaRepresentation = areaFactory.convertToAreas(drawing)

      Then("every rooms area should be in the grid")
      for (room <- areaRepresentation.rooms) {
        assert(grid.elementsIn(gridStart, gridStop).exists(e => e.start == room.start && e.stop == room.stop), "The room " + room + " was not found in the grid.")
      }

      And("every bend should be in the grid")
      for (bend <- areaRepresentation.bends) {
        assert(grid.elementsIn(gridStart, gridStop).exists(e => e.start == bend.start && e.stop == bend.stop), "The bend " + bend + " was not found in the grid.")
      }

      And("every corridor segment should be in the grid")
      for (corridor <- areaRepresentation.corridors; segment <- corridor.areas.filter(a => a.isCorridor)) {
        assert(grid.elementsIn(gridStart, gridStop).exists(e => e.start == segment.start && e.stop == segment.stop), "The corridor segment " + segment + " was not found in the grid.")
      }

    }

    it ("should add connections between corridors and rooms") {

      Given("a room that connects to a corridor segment to the west, and another one that connects to the north")
      val f = twoRoomsWithBend
      import f._

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("the first room should have a single connection to the west")
      val firstRoom = areaRepresentation.rooms.find(r => r.start == room1Start && r.stop == room1Stop).get.asInstanceOf[RoomArea]
      firstRoom.connection(West) should be ('defined)
      firstRoom.connection(North) should not be ('defined)
      firstRoom.connection(East) should not be ('defined)
      firstRoom.connection(South) should not be ('defined)

      And("the second room should have a single connection to the north")
      val secondRoom = areaRepresentation.rooms.find(r => r.start == room2Start && r.stop == room2Stop).get.asInstanceOf[RoomArea]
      secondRoom.connection(West) should not be ('defined)
      secondRoom.connection(North) should be ('defined)
      secondRoom.connection(East) should not be ('defined)
      secondRoom.connection(South) should not be ('defined)

    }

    it ("should add connections between corridors and bends") {

      Given("two bends connecting to two rooms, in direction room1 -> West -> bend1 -> South -> bend2 -> East -> room2")
      val f = twoRoomsWithTwoBends
      import f._

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      val bend1 = areaRepresentation.bends.find(b => b.start == Point(3, 0) && b.stop == Point(5, 2)).get.asInstanceOf[CorridorBend]
      val bend2 = areaRepresentation.bends.find(b => b.start == Point(3, 3) && b.stop == Point(5, 5)).get.asInstanceOf[CorridorBend]

      Then("bend 1 should have connections west and south")
      bend1.connection(West) should be ('defined)
      bend1.connection(South) should be ('defined)

      And("bend 2 should have connections north and east")
      bend2.connection(North) should be ('defined)
      bend2.connection(West) should be ('defined)

    }

    it ("should map every room and corridor to an entry in the representation") {

      Given("a drawing with a corridor connecting two rooms")
      val f = twoRoomsWithTwoBends
      import f._

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("both rooms should be contained in the room map with different areas")
      areaRepresentation.roomMap.keySet should contain (room1)
      areaRepresentation.roomMap.keySet should contain (room2)
      areaRepresentation.roomMap(room1) should not equal (areaRepresentation.roomMap(room2))

      And("the corridor should be contained in the corridor map")
      areaRepresentation.corridorMap should have size (1)
      areaRepresentation.corridorMap.keySet should contain (edge)

    }

    it ("should set every entry as intersecting") {

      Given("a drawing with rooms, corridors and bends")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(3, 5)
      val r1X = 1
      val r1Y = 0
      val r2X = 0
      val r2Y = 2
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val edge = ConstraintEdge(room1, room2, 3)
      val bends = Vector((0, 0))
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas that allows intersection")
      val intersectingAreas = intersectingAreaFactory.convertToAreas(drawing)

      Then("every segment should allow intersection")
      intersectingAreas.bends.foreach(b => assert(b.allowsIntersection, b + " did not allow intersection"))
      intersectingAreas.rooms.foreach(r => assert(r.allowsIntersection, r + " did not allow intersection"))
      intersectingAreas.corridors.foreach(c => c.toIterator.foreach(segment => assert(segment.allowsIntersection, segment + " did not allow intersection")))

    }

    it ("should set every entry as non-intersecting") {

      Given("a drawing with rooms, corridors and bends")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(3, 5)
      val r1X = 1
      val r1Y = 0
      val r2X = 0
      val r2Y = 2
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val edge = ConstraintEdge(room1, room2, 3)
      val bends = Vector((0, 0))
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

      val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)

      When("generating areas that does not allow intersection")
      val intersectingAreas = nonIntersectingFactory.convertToAreas(drawing)

      Then("every segment should allow intersection")
      intersectingAreas.bends.foreach(b => assert(!b.allowsIntersection, b + " allows intersection"))
      intersectingAreas.rooms.foreach(r => assert(!r.allowsIntersection, r + " allows intersection"))
      intersectingAreas.corridors.foreach(c => c.toIterator.foreach(segment => assert(!segment.allowsIntersection, segment + " did not allow intersection")))

    }

    it ("should throw an exception if the drawing lacks vertices") {

      Given("a drawing with no vertices")
      val f = twoRoomsWithTwoBends
      import f._
      val drawing = new OrthogonalLayout(Vector(drawnEdge), Vector())

      When("generating areas from the drawing")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        intersectingAreaFactory.convertToAreas(drawing)
      }

    }

    it ("should throw an exception if a room has a side with length less than 3") {

      Given("a room with length less than 3")
      val room = new ConstraintRoom(2, 5)
      val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room, 0, 0)))

      When("generating areas from the drawing")
      Then("an exception should be thrown")
      intercept[Error] {
        intersectingAreaFactory.convertToAreas(drawing)
      }

    }

    it ("should set the min(max coordinates to 0,0 and the maximum width/height of the grid") {

      Given("two 5x5 rooms aligned horizontally")
      val room1 = new ConstraintRoom(5, 5)
      val room2 = new ConstraintRoom(5, 5)
      val roomList = Vector((room1, 0, 0), (room2, 1, 0))
      val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), roomList)

      When("generating areas from the drawing")
      val areaRepresentation = intersectingAreaFactory.convertToAreas(drawing)

      Then("min x/y should be 0,0")
      areaRepresentation.min should be (Point(0, 0))

      Then("max x/y should be 9,4")
      areaRepresentation.max should be (Point(10, 5))

    }

  }

}
