package rldrawing.integration

import helperClasses.RandomMock
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.RandomElementFinderInterface
import net.cyndeline.rlgraph.drawings.planar.orthogonal.drawing.{DrawnEdge, OrthogonalLayout}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.{AreaCompaction, TargetFinder}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.{ImmutableGridDrawing, RepresentationToDrawingConverterI}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.MutableArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.{AreaRepresentation, OrthogonalAreaFactory}
import net.cyndeline.scalarlib.subcut.ProjectConfiguration
import org.scalatest.BeforeAndAfter
import rldrawing.help.{ConstraintEdge, ConstraintRoom}
import testHelpers.SpecImports

import scala.util.Random

class AreaCompactionSpec extends SpecImports with BeforeAndAfter {
  private val intersectingAreaFactory = new OrthogonalAreaFactory(true)

  ProjectConfiguration.modifyBindings { implicit module =>  // implicit makes the test module default
    import module._

    after {
      bind[RandomElementFinderInterface] to None
      bind[TargetFinder] to None
    }

    def makeCompaction(x: Int,
                       y: Int,
                       areas: AreaRepresentation[ConstraintRoom, ConstraintEdge[ConstraintRoom]]) = new {
      val randomCollectionFinder = mock[RandomElementFinderInterface]
      val targetFinder = target(x, y)
      bind[RandomElementFinderInterface] toProvider { randomCollectionFinder }
      bind[TargetFinder] toProvider { targetFinder }

      val randomArea = mock[MutableArea]
      (randomArea.area _) expects() returns(null) anyNumberOfTimes() // Avoids null pointers
      (randomCollectionFinder.findRandomElement[MutableArea] _) expects(*, *) returns(randomArea) once()

      val random = RandomMock()
      val compaction = new AreaCompaction(areas, random)
    }

    def target(x: Int, y: Int): TargetFinder = {
      val finderMock = mock[TargetFinder]
      (finderMock.findTarget _) expects(*) returns(Point(x, y)) once()
      finderMock
    }

    describe("AreaCompaction") {

      it ("should compact two areas horizontally") {

        Given("two 3x5 areas with distance between each other")
        val room1 = new ConstraintRoom(3, 6)
        val room2 = new ConstraintRoom(3, 6)
        val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room1, 0, 0), (room2, 1, 0)))
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the two areas to the point (0, 0")
        val f = makeCompaction(0, 0, areas); import f._
        compaction.completeCompact()

        Then("both areas should be moved to the left and intersect at the border")
        val area1 = areas.roomMap(room1)
        val area2 = areas.roomMap(room2)
        area1.start should be (Point(0, 0))
        area2.start should be (Point(2, 0))
        area1.stop should be (Point(2, 5))
        area2.stop should be (Point(4, 5))

      }

      it ("should compact two areas vertically") {

        Given("two 5x3 areas with distance between each other")
        val room1 = new ConstraintRoom(6, 3)
        val room2 = new ConstraintRoom(6, 3)
        val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room1, 0, 0), (room2, 0, 2)))
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the two areas to the point (0, 0")
        val f = makeCompaction(0, 0, areas); import f._
        compaction.completeCompact()

        Then("both areas should be moved up and intersect at the border")
        val area1 = areas.roomMap(room1)
        val area2 = areas.roomMap(room2)
        area1.start should be (Point(0, 0))
        area2.start should be (Point(0, 2))
        area1.stop should be (Point(5, 2))
        area2.stop should be (Point(5, 4))

      }

      it ("should stop areas once they reach the target") {

        Given("a 3x6 area and a target that lies above the stop value on the y axis")
        val room1 = new ConstraintRoom(6, 3)
        val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room1, 0, 0)))
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the area towards the target")
        val f = makeCompaction(1, 5, areas); import f._
        compaction.completeCompact()

        Then("the area should occupy the lower part of its 6x6 cell")
        val area1 = areas.roomMap(room1)
        area1.start should be (Point(0, 3))
        area1.stop should be (Point(5, 5))

      }

      it ("should compact corridors") {

        Given("two 3x6 rooms with a horizontal corridor with length 5 (width 6) going between them")
        val room1 = new ConstraintRoom(3, 6)
        val room2 = new ConstraintRoom(3, 6)
        val roomList = Vector((room1, 0, 0), (room2, 1, 0))
        val edge = ConstraintEdge(room1, room2, 6)
        val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 0), Vector(), edge)
        val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the area towards the left side of the first room")
        val f = makeCompaction(0, 0, areas); import f._
        compaction.completeCompact()

        Then("the corridor should have length 1")
        val corridor = areas.corridorMap(edge).areas.head // Straight corridor with a single segment
        corridor.area.lengthOfSide(North) should be (1)

        And("the corridor should start at the west room")
        val westRoom = areas.roomMap(room1)
        corridor.area.coordinatesOnSide(North)._1 should be (westRoom.area.coordinatesOnSide(North)._2)

        And("the corridor should stop at the east room")
        val eastRoom = areas.roomMap(room2)
        corridor.area.coordinatesOnSide(North)._2 should be (eastRoom.area.coordinatesOnSide(North)._1)

      }

      it ("should compact corridor bends") {

        Given("a 5x5 room at (0,0) and a 5x5 room at (1,1) with a corridor of width 3 having a bend at (1,0)")
        val room1 = new ConstraintRoom(5, 5)
        val room2 = new ConstraintRoom(5, 5)
        val roomList = Vector((room1, 0, 0), (room2, 1, 1))
        val edge = ConstraintEdge(room1, room2, 3, 1, 99) // min length 1, Max deviation = 99, allowing maximum compaction
        val bends = Vector((1, 0))
        val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 1), bends, edge)
        val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the areas towards the bottom of first room")
        val f = makeCompaction(4, 4, areas); import f._
        compaction.completeCompact()

        Then("the bend area should be adjacent to the lower east side of room 1")
        val bend = areas.bends.head // Only 1 bend, size 3x3
        bend.start should be (Point(4, 2))
        bend.stop should be (Point(6, 4))

        And("the corridor segments should have length 1")
        val room1ToBend = areas.corridors.head.areas(0)
        val bendToRoom2 = areas.corridors.head.areas(2)
        room1ToBend.area.lengthOfSide(North) should be (1)
        bendToRoom2.area.lengthOfSide(East) should be (1)

      }

      it ("should adhere to minimum corridor length when compacting rooms") {

        Given("two 3x6 rooms next to each other and a corridor with length 5 and minimum length 3")
        val room1 = new ConstraintRoom(3, 6)
        val room2 = new ConstraintRoom(3, 6)
        val roomList = Vector((room1, 0, 0), (room2, 1, 0))
        val edge = ConstraintEdge(room1, room2, 3, 3) // width, min length
        val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 0), Vector(), edge)
        val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the areas towards the inside of the first room")
        val f = makeCompaction(2, 2, areas); import f._
        compaction.completeCompact()

        Then("the corridor should retain length 3")
        val corridor = areas.corridorMap(edge).areas.head
        corridor.area.lengthOfSide(North) should be (3)

        And("room 2 should only be allowed to move 2 coordinates east before the corridor starts blocking")
        val room2Area = areas.roomMap(room2).area
        room2Area.start should be (Point(5, 0))
        room2Area.stop should be (Point(7, 5))

      }

      it ("should adhere to corridor deviation margins when compacting rooms") {

        Given("a 5x5 room at (0,0) and a 5x5 room at (1,1) with a corridor with margin 0 and maximum minimum length, having a bend at (1,0)")
        val room1 = new ConstraintRoom(5, 5)
        val room2 = new ConstraintRoom(5, 5)
        val roomList = Vector((room1, 0, 0), (room2, 1, 1))
        val edge = ConstraintEdge(room1, room2, 3, 99, 0) // min length 99, Max deviation = 0, allowing no compaction
        val bends = Vector((1, 0))
        val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 1), bends, edge)
        val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("compacting the areas towards the bottom of first room")
        val f = makeCompaction(4, 4, areas); import f._
        compaction.completeCompact()

        Then("neither area should be able to move")
        val area1 = areas.roomMap(room1)
        val area2 = areas.roomMap(room2)
        val bend = areas.bends.head
        area1.start should be (Point(0, 0))
        area1.stop should be (Point(4, 4))
        area2.start should be (Point(5, 5))
        area2.stop should be (Point(9, 9))
        bend.start should be (Point(6, 1))
        bend.stop should be (Point(8, 3))

      }

      it ("should use an area from a room or a bend when submitting the target area") {

        Given("two rooms and a corridor with a bend")
        val room1 = new ConstraintRoom(5, 5)
        val room2 = new ConstraintRoom(5, 5)
        val roomList = Vector((room1, 0, 0), (room2, 1, 1))
        val edge = ConstraintEdge(room1, room2, 3, 1, 99) // min length 1, Max deviation = 99, allowing maximum compaction
        val bends = Vector((1, 0))
        val drawnEdge = new DrawnEdge(room1, room2, (0, 0), (1, 1), bends, edge)
        val drawing = new OrthogonalLayout(Vector(drawnEdge), roomList)
        val areas = intersectingAreaFactory.convertToAreas(drawing)

        When("creating the compaction algorithm")
        Then("the random element finder should receive all rooms and bends")
        val randomCollectionFinder = mock[RandomElementFinderInterface]
        val randomRoom = areas.rooms.head
        (randomCollectionFinder.findRandomElement[MutableArea] _) expects(areas.rooms union areas.bends, *) returns(randomRoom) once()

        And("the target finder should receive the area from the result selected by the element finder")
        val targetFinder = mock[TargetFinder]
        (targetFinder.findTarget _) expects(randomRoom.area) returns(Point(55, 66)) once()

        bind[RandomElementFinderInterface] toProvider { randomCollectionFinder }
        bind[TargetFinder] toProvider { targetFinder }

        // Start test
        new AreaCompaction(areas, RandomMock())

      }

      it ("should produce an immutable drawing of its current area representation") {

        Given("a representation")
        val room1 = new ConstraintRoom(3, 6)
        val room2 = new ConstraintRoom(3, 6)
        val drawing = new OrthogonalLayout[ConstraintRoom, ConstraintEdge](Vector(), Vector((room1, 0, 0), (room2, 1, 0)))
        val representation = intersectingAreaFactory.convertToAreas(drawing)

        When("producing an immutable drawing with the compaction algorithm")
        val random = new Random()
        val converterMock = mock[RepresentationToDrawingConverterI]
        bind[RepresentationToDrawingConverterI] toProvider { converterMock }
        val compaction = new AreaCompaction(representation, random)

        Then("the current representation should be used")
        val result = new ImmutableGridDrawing[ConstraintRoom, ConstraintEdge[ConstraintRoom]](Set(), Set())
        (converterMock.convertToDrawing[ConstraintRoom, ConstraintEdge[ConstraintRoom]] _) expects(representation) returns(result) once()

        // Start test
        val returnedResult = compaction.produceCompactedDrawing

        And("the result should be returned")
        returnedResult should be (result)

      }

      it ("should throw an exception if the drawing contains no rooms") {

        Given("a drawing with corridors and no rooms")
        val representation = AreaRepresentation[ConstraintRoom, ConstraintEdge[ConstraintRoom]](Set(), Set(), Set(), Map(), Map(), Point(0, 0), Point(10, 10))

        When("creating the compaction algorithm")
        Then("an exception should be thrown")
        intercept[IllegalArgumentException] {
          new AreaCompaction(representation, new Random())
        }

      }
    }
  } // DI
}
