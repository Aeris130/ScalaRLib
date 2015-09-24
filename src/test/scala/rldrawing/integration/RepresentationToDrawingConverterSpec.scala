package rldrawing.integration

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import rldrawing.help.{ConstraintEdge, ConstraintRoom}
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.orthogonal.drawing.{DrawnEdge, OrthogonalLayout}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.OrthogonalAreaFactory
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.RepresentationToDrawingConverter
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{MutableArea, RectangularArea}
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.Connection

@RunWith(classOf[JUnitRunner])
class RepresentationToDrawingConverterSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val intersectingAreaFactory = new OrthogonalAreaFactory(true)
  private val converter = new RepresentationToDrawingConverter()

  /**
   * A 5x5 room at coordinate 0,1 with a bending corridor to a 3x3 room at 1,0.
   */
  def twoRoomsWithBend = new {
    val room1 = new ConstraintRoom(5, 5)
    val room2 = new ConstraintRoom(3, 3)
    val r1X = 0
    val r1Y = 0
    val r2X = 1
    val r2Y = 1
    val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

    val edge = ConstraintEdge(room1, room2, 3)
    val bends = Vector((1, 0))
    val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), bends, edge)

    val orthogonalDrawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
    val areaRepresentation = intersectingAreaFactory.convertToAreas(orthogonalDrawing)
  }

  def twoRoomsWithoutBend = new {
    val room1 = new ConstraintRoom(5, 5)
    val room2 = new ConstraintRoom(3, 3)
    val r1X = 0
    val r1Y = 0
    val r2X = 1
    val r2Y = 0
    val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

    val edge = ConstraintEdge(room1, room2, 3)
    val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), Vector(), edge)

    val orthogonalDrawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
    val areaRepresentation = intersectingAreaFactory.convertToAreas(orthogonalDrawing)
  }

  describe("RepresentationToDrawingConverter") {

    /*
     * Note that most of these tests use a room of maximum size (both height and width) in the upper left corner at
     * (0, 0), this is so that no areas will have their coordinates adjusted and remain consistent with the original
     * representation, making it possible to write tests without specifying exact coordinates.
     */

    it ("should convert mutable room areas to rectangles along with their original rooms") {

      Given("a drawing with two rooms")
      val room1 = new ConstraintRoom(6, 6)
      val room2 = new ConstraintRoom(5, 6)
      val roomList = Vector((room1, 0, 0), (room2, 1, 0))
      val orthoDrawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), roomList)
      val areaRepresentation = intersectingAreaFactory.convertToAreas(orthoDrawing)

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("room 1 should have an entry with an area matching the original rooms in the representation")
      val room1Area = areaRepresentation.roomMap(room1).area
      val room1Drawing = drawing.rooms.find(r => r.originalRoom == room1).getOrElse {
        fail("No original room matching " + room1 + " found in the drawing.")
      }
      room1Drawing.area should equal (room1Area)

      And("room 2 should have an entry with an area matching the original rooms in the representation")
      val room2Area = areaRepresentation.roomMap(room2).area
      val room2Drawing = drawing.rooms.find(r => r.originalRoom == room2).getOrElse {
        fail("No original room matching " + room2 + " found in the drawing.")
      }
      room2Drawing.area should equal (room2Area)

      And("no other room entries should be found")
      drawing.rooms.size should be (2)

    }

    it ("should convert edges without bends to rectangles") {

      Given("two rooms connected by an edge without bends")
      val f = twoRoomsWithoutBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the drawing should contain a corridor entry with an area matching the mutable corridor")
      val originalCorridor = areaRepresentation.corridorMap(edge)
      val drawnCorridor = drawing.corridors.find(c => c.originalCorridor == edge).getOrElse {
        fail("No original corridor matching " + edge + " found in the drawing.")
      }

      And("the corridor should contain a single segment with the same coordinates as the original")
      drawnCorridor.corridorSegments should have size 1
      drawnCorridor.corridorSegments.head.start should be (originalCorridor.areas.head.start)
      drawnCorridor.corridorSegments.head.stop should be (originalCorridor.areas.head.stop)

    }

    it ("should convert edges with bends to rectangles") {

      Given("two rooms connected by an edge with a bend")
      val f = twoRoomsWithBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the drawing should contain a corridor entry with the same amount of segments as the original")
      val originalCorridor = areaRepresentation.corridorMap(edge)
      val drawnCorridor = drawing.corridors.find(c => c.originalCorridor == edge).getOrElse {
        fail("No original corridor matching " + edge + " found in the drawing.")
      }
      drawnCorridor.corridorSegments should have size originalCorridor.areas.size
      drawnCorridor.corridorSegments should have size 3

      And("the first drawn segment should match the coordinates of the first original segment")
      val firstDrawnSegment = drawnCorridor.corridorSegments(0)
      firstDrawnSegment.start should equal (originalCorridor.areas(0).start)
      firstDrawnSegment.stop should equal (originalCorridor.areas(0).stop)

      And("the second drawn segment should match the coordinates of the second original segment")
      val secondDrawnSegment = drawnCorridor.corridorSegments(1)
      secondDrawnSegment.start should equal (originalCorridor.areas(1).start)
      secondDrawnSegment.stop should equal (originalCorridor.areas(1).stop)

      And("the third drawn segment should match the coordinates of the third original segment")
      val thirdDrawnSegment = drawnCorridor.corridorSegments(2)
      thirdDrawnSegment.start should equal (originalCorridor.areas(2).start)
      thirdDrawnSegment.stop should equal (originalCorridor.areas(2).stop)

    }

    it ("should add the original edge when converting edges") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the entry for the edge should contain a reference to the original edge itself")
      assert(drawing.corridors.exists(entry => entry.originalCorridor == edge), "No entry with the original corridor " + edge + " found.")

    }

    it ("should compute connections as start/stop points when converting edges without segments") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the room1 -> corridor connection coordinates should equal the intersection between the first room and the corridor segment")
      val room1Area = areaRepresentation.roomMap(room1).area
      val corridor = drawing.corridors.head
      val corridorArea = RectangularArea(corridor.corridorSegments.head.start, corridor.corridorSegments.head.stop)

      val startIntersection = corridorArea.intersection(room1Area).get
      corridor.startConnection should be (Connection(startIntersection.start, startIntersection.stop))

      And("the corridor -> room2 connection coordinates should equal the intersection between the last room and the corridor segment")
      val room2Area = areaRepresentation.roomMap(room2).area
      val stopIntersection = corridorArea.intersection(room2Area).get
      corridor.stopConnection should be (Connection(stopIntersection.start, stopIntersection.stop))

    }

    it ("should compute connections as start/stop points when converting edges with segments") {

      Given("two rooms connected by an edge with bends")
      val f = twoRoomsWithBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("two corridor connections should exist")
      val corridor = drawing.corridors.head
      corridor.corridorConnections should have size 2

      Then("the first corridor -> bend connection should be the intersection between the first corridor and bend segment")
      val segment1Area = areaRepresentation.corridorMap(edge).areas(0).area
      val bendArea = areaRepresentation.corridorMap(edge).areas(1).area
      val intersection1 = segment1Area.intersection(bendArea).get
      corridor.corridorConnections(0) should be (Connection(intersection1.start, intersection1.stop))

      And("the second bend -> corridor connection should be the intersection between the bend segment and the second corridor segment")
      val segment2Area = areaRepresentation.corridorMap(edge).areas(2).area
      val intersection2 = segment2Area.intersection(bendArea).get
      corridor.corridorConnections(1) should be (Connection(intersection2.start, intersection2.stop))

    }

    it ("should retain the original rooms connected by a corridor") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the corridor segment should contain references to the original room pair")
      val corridor = drawing.corridors.head
      corridor.start should be (room1)
      corridor.stop should be (room2)

    }

    it ("should retain the original edge the corridor is based upon") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the corridor segment should contain references to the original corridor")
      val corridor = drawing.corridors.head
      corridor.originalCorridor should be (edge)

    }

    it("should adjust area coordinates towards (0, 0) if no area occupies the coordinate 0 on any axis") {

      Given("a drawing with two rooms and an edge where the room closest to (0, 0) starts at (1, 2) and ends at (5, 4)")
      val room1 = new ConstraintRoom(5, 3)
      val room2 = new ConstraintRoom(7, 3)
      val r1X = 0
      val r1Y = 0
      val r2X = 1
      val r2Y = 0
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val edge = ConstraintEdge(room1, room2, 3)
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), Vector(), edge)

      val orthogonalDrawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
      val areaRepresentation = intersectingAreaFactory.convertToAreas(orthogonalDrawing)

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the first room should be adjusted towards (0, 0) by 1 on the x axis and 2 on the y axis")
      val room1Area = areaRepresentation.roomMap(room1).area
      val room1Drawing = drawing.rooms.find(r => r.originalRoom == room1).getOrElse {
        fail("No original room matching " + room1 + " found in the drawing.")
      }
      val area1AfterAdjustment = room1Area.adjustCoordinates(West, 1).adjustCoordinates(North, 2)
      room1Drawing.area should equal (area1AfterAdjustment)

      And("the second should be adjusted towards (0, 0) by 1 on the x axis and 2 on the y axis")
      val room2Area = areaRepresentation.roomMap(room2).area
      val room2Drawing = drawing.rooms.find(r => r.originalRoom == room2).getOrElse {
        fail("No original room matching " + room2 + " found in the drawing.")
      }
      val area2AfterAdjustment = room2Area.adjustCoordinates(West, 1).adjustCoordinates(North, 2)
      room2Drawing.area should equal (area2AfterAdjustment)

      And("the corridor segment should be adjusted towards (0, 0) by 1 on the x axis and 2 on the y axis")
      val corridorArea = areaRepresentation.corridorMap(edge).areas.head.area
      val corridorDrawing = drawing.corridors.find(c => c.originalCorridor == edge).getOrElse {
        fail("No original corridor matching " + edge + " found in the drawing.")
      }
      val corridorAfterAdjustment = corridorArea.adjustCoordinates(West, 1).adjustCoordinates(North, 2)
      corridorDrawing.corridorSegments.head should equal (corridorAfterAdjustment)

    }

    it ("should adjust connection coordinates towards (0, 0) if no area occupies the coordinate 0 on any axis") {

      Given("a drawing with two rooms and an edge where the room closest to (0, 0) starts at (1, 2) and ends at (5, 4)")
      val room1 = new ConstraintRoom(5, 3)
      val room2 = new ConstraintRoom(7, 3)
      val r1X = 0
      val r1Y = 0
      val r2X = 1
      val r2Y = 0
      val roomList = Vector((room1, r1X, r1Y), (room2, r2X, r2Y))

      val edge = ConstraintEdge(room1, room2, 3)
      val drawnEdge = new DrawnEdge(room1, room2, (r1X, r1Y), (r2X, r2Y), Vector(), edge)

      val orthogonalDrawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
      val areaRepresentation = intersectingAreaFactory.convertToAreas(orthogonalDrawing)

      When("converting the representation to a drawing")
      val drawing = converter.convertToDrawing(areaRepresentation)

      Then("the intersection of the first room and the corridor segment should be adjusted towards (0, 0) by 1 on the x axis and 2 on the y axis")
      val corridorSegment: MutableArea = areaRepresentation.corridors.head.areas.head
      val originalRoom1 = areaRepresentation.roomMap(room1)
      val originalIntersection1 = originalRoom1.area.intersection(corridorSegment.area).get

      val adjustedConnection1 = originalIntersection1.adjustCoordinates(West, 1).adjustCoordinates(North, 2)
      drawing.corridors.head.startConnection should be (Connection(adjustedConnection1.start, adjustedConnection1.stop))

      And("the intersection of the corridor and the second room should be adjusted towards (0, 0) by 1 on the x axis and 2 on the y axis")
      val originalRoom2 = areaRepresentation.roomMap(room2)
      val originalIntersection2 = originalRoom2.area.intersection(corridorSegment.area).get

      val adjustedConnection2 = originalIntersection2.adjustCoordinates(West, 1).adjustCoordinates(North, 2)
      drawing.corridors.head.stopConnection should be (Connection(adjustedConnection2.start, adjustedConnection2.stop))

    }

    it ("should throw an exception if two corridor segments does not intersect") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("separating the corridor and second room from the first room by 1 coordinate")
      val secondRoom = areaRepresentation.roomMap(room2)
      val corridor = areaRepresentation.corridorMap(edge)
      secondRoom.markAsMoved(East)  // Room 2 is moved to keep its position with the corridor valid
      secondRoom.move()
      corridor.areas.head.markAsMoved(East)
      corridor.areas.head.move()

      Then("an error should be thrown")
      intercept[Error] {
        converter.convertToDrawing(areaRepresentation)
      }

    }

    it ("should throw an exception if two corridor segments intersect inside each others borders") {

      Given("two rooms connected by an edge")
      val f = twoRoomsWithoutBend
      import f._

      When("overlapping the corridor with the first room by 2 coordinates instead of 1")
      val firstRoom = areaRepresentation.roomMap(room1)
      val secondRoom = areaRepresentation.roomMap(room2)
      val corridor = areaRepresentation.corridorMap(edge)
      firstRoom.markAsMoved(West) // Room 1 is marked so the corridor moves by adjusting coordinates instead of shrinking
      secondRoom.markAsMoved(West) // Room 2 is moved to keep its position with the corridor valid
      secondRoom.move()
      corridor.areas.head.markAsMoved(West)
      corridor.areas.head.move()

      Then("an error should be thrown")
      intercept[Error] {
        converter.convertToDrawing(areaRepresentation)
      }

    }

  }
}
