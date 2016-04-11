package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{HeightConstraint, WidthConstraint}
import net.cyndeline.rlgraph.planarGraphDrawing.orthogonal.OrthogonalRepresentation
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.help.ConnectionBoundary
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{AdjustableRectangle, _}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.{CorridorProperties, PartitionedArea}

import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Builds a grid of areas representing rooms and corridors by assigning each area coordinates on the grid based
 * on where in the drawing the area is positioned. Each coordinate in the drawing is converted to an MxN grid, where
 * M and N is the largest side of any room in the drawing. Corridors with bends are represented as a sequence of
 * straight segments that connect to a corridor bend area with width and height equal to the width of the corridor.
 *
 * Constructs a new orthogonal area factory with a user-supplied partition factory.
 *
 * @param partitionFactory Factory responsible for producing the grid used in mutable areas when checking for
 *                         collisions.
 * @param intersect True if non-connected areas should be allowed to intersect each other at their borders, otherwise
 *                  false.
 */
class OrthogonalAreaFactory(partitionFactory: GridPartitionFactory[MutableArea], intersect: Boolean) {
  private val corridorSegmentFactory = new CorridorSegmentAreaFactory()
  private val connectionFactory = new ConnectionFactory()

  /**
   * @constructor Constructs a new orthogonal area factory.
   * @param intersect True if non-connected areas should be allowed to intersect each other at their borders, otherwise
   *                  false.
   * @return A new orthogonal area factory.
   */
  def this(intersect: Boolean) = this(new DefaultPartitionFactory(), intersect)

  /**
   * Converts an orthogonal drawing into a set of connected areas on a grid.
 *
   * @param drawing An orthogonal drawing of a map.
   * @tparam VType Room type in the map.
   * @tparam EType Corridor type in the map.
   */
  def convertToAreas[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X] with CorridorProperties]
  (drawing: OrthogonalRepresentation[VType, EType]): AreaRepresentation[VType, EType[VType]] = {
    if (drawing.vertices.isEmpty)
      throw new IllegalArgumentException("Cannot parse an orthogonal drawing with no vertices into an area set.")

    val roomGrid: Array[Array[Option[MutableArea]]] = makeEmptyGrid(drawing)
    val largestRoomSide = findLargestSide(drawing)

    // A grid of length n will have absolute coordinates from 0 to n - 1
    val partitionedGrid = partitionFactory.createGrid(Point(0, 0), Point(roomGrid.length * largestRoomSide, roomGrid(0).length * largestRoomSide), drawing.vertices.size)
    val allRooms = addRooms(roomGrid, partitionedGrid, drawing, largestRoomSide)
    val corridorsAndBends = addCorridors(roomGrid, partitionedGrid, drawing, largestRoomSide)
    AreaRepresentation(allRooms.values.toSet, corridorsAndBends._2, corridorsAndBends._1.values.toSet, allRooms, corridorsAndBends._1, partitionedGrid.start, partitionedGrid.stop)
  }

  /**
   * Creates a grid where each cell holds a single room/bend area.
   */
  private def makeEmptyGrid[VType, EType[X] <: UnDiEdge[X]](drawing: OrthogonalRepresentation[VType, EType]): Array[Array[Option[MutableArea]]] = {
    var x = 0
    var y = 0

    /* Only check bends, rooms are processed below. */
    for (segment <- drawing.edges; bend: (Int, Int) <- segment.bends) {
      if (bend._1 > x) x = bend._1
      if (bend._2 > y) y = bend._2
    }

    for (roomAndCoordinate <- drawing.vertices) {
      val xCoordinate = roomAndCoordinate._2
      val yCoordinate = roomAndCoordinate._3
      if (xCoordinate > x) x = xCoordinate
      if (yCoordinate > y) y = yCoordinate
    }

    Array.fill[Option[MutableArea]](x + 1, y + 1) { None }
  }

  /**
   * Finds the single largest side of any room in the drawing.
   */
  private def findLargestSide[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X] with CorridorProperties]
  (drawing: OrthogonalRepresentation[VType, EType]): Int = {
    var largest = 0

    for (roomAndCoordinate <- drawing.vertices) {
      val room = roomAndCoordinate._1
      if (room.elementHeight > largest) largest = room.elementHeight
      if (room.elementWidth > largest) largest = room.elementWidth

      if (room.elementHeight < 3 || room.elementWidth < 3)
        throw new Error("Room " + room + " currently has height " + room.elementHeight + " and width " + room.elementWidth + ", height and width must be equal to or higher than 3.")
    }

    largest
  }

  /**
   * Computes rooms and adds them to a grid, then returns the set of all rooms.
   */
  private def addRooms[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X] with CorridorProperties]
  (roomGrid: Array[Array[Option[MutableArea]]],
   partitionedGrid: PartitionedArea[MutableArea],
   drawing: OrthogonalRepresentation[VType, EType],
   largestRoomSide: Int): Map[VType, MutableArea] = {

    var roomMap = Map[VType, MutableArea]()

    /* Begin by building a room for each vertex in the drawing */
    for (roomAndCoordinates <- drawing.vertices) {
      val room = roomAndCoordinates._1
      val coordinate = (roomAndCoordinates._2, roomAndCoordinates._3)
      val startStop = computeRectangularAreaInMiddle(coordinate, room.elementWidth, room.elementHeight, largestRoomSide)
      val roomArea = new RoomArea(AdjustableRectangle(startStop._1, startStop._2), partitionedGrid, intersect)

      roomGrid(roomAndCoordinates._2)(roomAndCoordinates._3) = Option(roomArea)
      roomMap += (room -> roomArea)
      partitionedGrid.add(roomArea)
    }

    roomMap
  }

  /**
   * Creates corridor containers and also separates bends into a separate set.
   */
  private def addCorridors[VType <: WidthConstraint with HeightConstraint, EType[X] <: UnDiEdge[X] with CorridorProperties]
  (roomGrid: Array[Array[Option[MutableArea]]],
   partitionedGrid: PartitionedArea[MutableArea],
   drawing: OrthogonalRepresentation[VType, EType],
   largestRoomSide: Int): (Map[EType[VType], FullCorridor], Set[MutableArea]) = {

    var fullCorridors = Map[EType[VType], FullCorridor]()
    val allBends = new ArrayBuffer[MutableArea]()

    for (edge <- drawing.edges) {
      var currentStart: (Int, Int) = edge.startPos
      var currentArea = roomGrid(currentStart._1)(currentStart._2).getOrElse {
        throw new Error("Every room area must be instantiated before connecting corridors.")
      }
      val corridorStart = currentArea // Added to the final container
      var segmentList = Vector[MutableArea]()
      val bends = edge.bends.toIterator

      while(bends.hasNext) {
        val bend: (Int, Int) = bends.next()
        val lengthOfBendSides = edge.originalEdge.elementWidth // The bend is equal to the corridors width on both sides

        // The final coordinates for the bend area
        val pointsOfBend = computeRectangularAreaInMiddle(bend, lengthOfBendSides, lengthOfBendSides, largestRoomSide)

        // The direction the corridor segment is outgoing from, seen from the previous area
        val connectDir = connectionDirection(Point(currentStart._1, currentStart._2), Point(bend._1, bend._2))
        val newBendArea = new CorridorBend(AdjustableRectangle(pointsOfBend._1, pointsOfBend._2), partitionedGrid, intersect)

        val corridorArea = corridorSegmentFactory.makeSegment(currentArea.area, connectDir, newBendArea.area, edge.originalEdge.elementWidth)
        val corridor = makeCorridorArea(corridorArea, partitionedGrid, connectDir, edge.originalEdge)

        connectionFactory.connect(currentArea, corridor, newBendArea, connectDir, edge.originalEdge.boundaryDeviation)

        // Add the new corridor and the new segment to the final corridor segment list
        segmentList = segmentList :+ corridor
        segmentList = segmentList :+ newBendArea
        allBends += newBendArea

        partitionedGrid.add(corridor)
        partitionedGrid.add(newBendArea)

        // Update previous area to the current one
        currentStart = bend
        currentArea = newBendArea
      }

      // End by manually connecting the final bend to the last room in the corridor
      val lastRoom = roomGrid(edge.stopPos._1)(edge.stopPos._2).getOrElse {
        throw new Error("Every room area must be instantiated before connecting corridors.")
      }

      val connectDir = connectionDirection(Point(currentStart._1, currentStart._2), Point(edge.stopPos._1, edge.stopPos._2))
      val corridorArea = corridorSegmentFactory.makeSegment(currentArea.area, connectDir, lastRoom.area, edge.originalEdge.elementWidth)
      val corridor = makeCorridorArea(corridorArea, partitionedGrid, connectDir, edge.originalEdge)

      connectionFactory.connect(currentArea, corridor, lastRoom, connectDir, edge.originalEdge.boundaryDeviation)

      segmentList = segmentList :+ corridor
      partitionedGrid.add(corridor)

      fullCorridors += (edge.originalEdge -> new FullCorridor(corridorStart, lastRoom, segmentList))
    }

    (fullCorridors, allBends.toSet)
  }

  /**
   * Computes the start and stop points for a rectangle that is positioned in the middle of an area whose coordinates
   * are decided by which coordinate it belongs to in a grid, and how many NxN tiles each coordinate hosts (N =
   * the longest side).
   */
  private def computeRectangularAreaInMiddle(coordinate: (Int, Int),
                                             areaWidth: Int,
                                             areaHeight: Int,
                                             largestRoomSide: Int): (Point, Point) = {
    val gridPoint = Point(coordinate._1, coordinate._2)

    /* Compute where the middle is on both axises by borrowing the boundary algorithm and use it with an area of
     * width 1 along the axis.
     */

    // The upper left point in the help areas, the same for both axises
    val upperLeft = Point(gridPoint.x * largestRoomSide, gridPoint.y * largestRoomSide)

    // An area representing all tiles in the drawing coordinate, equal to the square size of the longest room side
    val lowerRight = Point((gridPoint.x * largestRoomSide) + largestRoomSide - 1, (gridPoint.y * largestRoomSide) + largestRoomSide - 1)
    val gridArea = AdjustableRectangle(upperLeft, lowerRight)
    val connectionBoundaryAlg = new ConnectionBoundary(gridArea)

    // Width
    val widthStop = Point(upperLeft.x + areaWidth - 1, upperLeft.y)
    val helpAreaWidth = AdjustableRectangle(upperLeft, widthStop)
    val middleStartStopXAxis = connectionBoundaryAlg.computeConnectionBoundary(helpAreaWidth, North)

    // Height
    val heightStop = Point(upperLeft.x, upperLeft.y + areaHeight - 1)
    val helpAreaHeight = AdjustableRectangle(upperLeft, heightStop)
    val middleStartStopYAxis = connectionBoundaryAlg.computeConnectionBoundary(helpAreaHeight, West)

    val roomStart = Point(middleStartStopXAxis._1, middleStartStopYAxis._1)
    val roomStop = Point(middleStartStopXAxis._2, middleStartStopYAxis._2)

    (roomStart, roomStop)
  }

  /**
   * Compares two points orthogonally positioned from each other (both have the same value on one axis, but differ on
   * the other), and returns the direction you have to traverse to get from the first point to the other.
   */
  private def connectionDirection(from: Point, to: Point): Direction = {
    if (from.x > to.x) West
    else if (from.x < to.x) East
    else if (from.y > to.y) North
    else if (from.y < to.y) South
    else throw new Error("Points " + from + " and " + to + " weren't positioned orthogonally from each other.")
  }

  /**
   * Creates a corridor area and sets the minimum length to the length of the corridor if its length is less than
   * the original corridors specified minimum length.
   */
  private def makeCorridorArea[VType, EType[X] <: UnDiEdge[X] with CorridorProperties](area: AdjustableRectangle,
                                                                                       grid: PartitionedArea[MutableArea],
                                                                                       direction: Direction,
                                                                                       original: EType[VType]): CorridorArea = {
    val minLength = if (area.lengthOfSide(direction.parallel) >= original.minimumLength)
      original.minimumLength
    else
      area.lengthOfSide(direction.parallel)

    new CorridorArea(area, grid, direction, minLength, intersect)
  }
}
