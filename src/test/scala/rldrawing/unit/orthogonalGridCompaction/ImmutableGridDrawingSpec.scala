package rldrawing.unit.orthogonalGridCompaction

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Rectangle
import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.{DrawnCorridor, ImmutableGridDrawing}
import testHelpers.SpecImports

import scalax.collection.GraphEdge.UnDiEdge

class ImmutableGridDrawingSpec extends SpecImports {

  private def roomWithCorridorToTheLeft = new {
    val room = mockRoom(Point(4, 3), Point(6, 6))
    val corridor = mockCorridor((Point(2, 3), Point(5, 6))) // <- Lowest x, y value
  }

  private def roomWithCorridorToTheRight = new {
    val room = mockRoom(Point(4, 3), Point(6, 6))
    val corridor = mockCorridor((Point(5, 4), Point(8, 9)))
  }

  private def roomWithCorridorAbove = new {
    val room = mockRoom(Point(5, 5), Point(12, 12))
    val corridor = mockCorridor((Point(6, 3), Point(9, 6)))
  }

  private def roomWithCorridorBelow = new {
    val room = mockRoom(Point(5, 4), Point(12, 12))
    val corridor = mockCorridor((Point(6, 11), Point(9, 14)))
  }

  describe("ImmutableGridDrawing") {

    it ("should compute the minimal x coordinate if it is a corridor segment") {

      Given("a room and a corridor, with the corridor having a lower x coordinate (2) than the room")
      val f = roomWithCorridorToTheLeft
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("2 should be the lowest x value")
      drawing.minX should be (2)

    }

    it ("should compute the minimal x coordinate if it is a room") {

      Given("a room and a corridor, with the room having a lower x coordinate (4) than the room")
      val f = roomWithCorridorToTheRight
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("4 should be the lowest x value")
      drawing.minX should be (4)

    }

    it ("should compute the minimal y coordinate if it is a corridor segment") {

      Given("a room and a corridor, with the corridor having a lower y coordinate (3) than the room")
      val f = roomWithCorridorAbove
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("3 should be the lowest y value")
      drawing.minY should be (3)

    }

    it ("should compute the minimal y coordinate if it is a room") {

      Given("a room and a corridor, with the room having a lower y coordinate (4) than the corridor")
      val f = roomWithCorridorBelow
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("4 should be the lowest y value")
      drawing.minY should be (4)

    }

    it ("should compute the maximal x coordinate if it is a corridor") {

      Given("a room and a corridor, with the corridor having a higher x coordinate (8) than the room")
      val f = roomWithCorridorToTheRight
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("8 should be the highest x value")
      drawing.maxX should be (8)

    }

    it ("should compute the maximal x coordinate if it is a room") {

      Given("a room and a corridor, with the corridor having a higher x coordinate (6) than the room")
      val f = roomWithCorridorToTheLeft
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("6 should be the highest x value")
      drawing.maxX should be (6)

    }

    it ("should compute the maximal y coordinate if it is a corridor") {

      Given("a room and a corridor, with the corridor having a higher y coordinate (14) than the room")
      val f = roomWithCorridorBelow
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("14 should be the highest y value")
      drawing.maxY should be (14)

    }

    it ("should compute the maximal y coordinate if it is a room") {

      Given("a room and a corridor, with the room having a higher y coordinate (12) than the corridor")
      val f = roomWithCorridorAbove
      import f._

      When("constructing a drawing")
      val drawing = ImmutableGridDrawing[Int, UnDiEdge[Int]](Set(room), Set(corridor))

      Then("12 should be the highest y value")
      drawing.maxY should be (12)

    }

  }

  private def mockRoom(from: Point, to: Point): DrawnRoom[Int] = {
    val room = mock[DrawnRoom[Int]]
    val area = mock[Rectangle]
    (area.start _ ) expects() returns(from) anyNumberOfTimes()
    (area.stop _ ) expects() returns(to) anyNumberOfTimes()
    (room.area _) expects() returns(area) anyNumberOfTimes()

    room
  }

  private def mockCorridor(segments: (Point, Point)*): DrawnCorridor[Int, UnDiEdge[Int]] = {
    val corridor = mock[DrawnCorridor[Int, UnDiEdge[Int]]]
    val segmentMocks = segments.toVector.map(s => {
      val m = mock[Rectangle]
      (m.start _ ) expects() returns(s._1) anyNumberOfTimes()
      (m.stop _ ) expects() returns(s._2) anyNumberOfTimes()
      m
    })
    (corridor.corridorSegments _) expects() returns(segmentMocks) anyNumberOfTimes()
    corridor
  }

}
