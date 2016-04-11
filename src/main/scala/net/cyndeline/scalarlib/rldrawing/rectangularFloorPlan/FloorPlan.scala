package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{Geom, Intersection}
import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.ModifiedCoordinates
import net.cyndeline.scalarlib.rldrawing.util.Connection

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Parses a rectangular dual layout into a floor plan consisting of rectangular room areas and the intersecting
 * intervals that the room shares with its (in the graph) connected neighbors.
 *
 * @param roomAreas Every area in the drawing, scaled such that the original layout remains intact while also having
 *                  each rectangle be equal or bigger than its corresponding vertex width/height constraints.
 */
class FloorPlan[V <: MapArea, E[X] <: UnDiEdge[X]] private (val roomAreas: Vector[RoomArea[V]], rLayout: RectangularLayout[V, E]) {
  checkForIntersectionOverlap()
  private val maxXYCoordinates = (maxXY, minXY)

  // The number of coordinates that two neighbors must share with each other.
  private val minimumIntersectionLength = 3

  /** Every room area in this drawing, mapped against every neighbors intersecting interval. */
  val intersections: Map[RoomArea[V], Vector[FloorIntersection[V]]] = computeIntersections

  val maxX: Int = maxXYCoordinates._1._1
  val maxY: Int = maxXYCoordinates._1._2
  val minX: Int = maxXYCoordinates._2._1
  val minY: Int = maxXYCoordinates._2._2

  /**
   * Rotates every coordinate in the drawing 90 degrees clockwise (assuming (0,0) is in the upper left corner of the
   * drawing).
 *
   * @return A copy of this floor plan with every coordinate rotated 90 degrees around the lowest starting point in
   *         the original floor plan.
   */
  def rotate90Clockwise: FloorPlan[V, E] = {
    val degree = 90
    val rotatedRooms = roomAreas.map(area => (area, rotatePoint(area.start, degree), rotatePoint(area.stop, degree)))
    val smallestX = findMinX(rotatedRooms)
    val smallestY = findMinY(rotatedRooms)
    val xDifference = Math.abs(rLayout.minX - smallestX)
    val yDifference = Math.abs(rLayout.minY - smallestY)
    val xAdd = if (smallestX < rLayout.minX) xDifference else -xDifference
    val yAdd = if (smallestY < rLayout.minY) yDifference else -yDifference
    val newRooms = rotatedRooms.map(r => {
      val newStart = Point(r._2._1 + xAdd, r._2._2 + yAdd)
      val newStop = Point(r._3._1 + xAdd, r._3._2 + yAdd)

      // Change x values since the points now point from the upper right corner to the lower left
      r._1.withNewCoordinates(Point(newStop.x, newStart.y), Point(newStart.x, newStop.y))
    })

    new FloorPlan(newRooms, rLayout)
  }

  /**
   * Rotates every coordinate in the drawing 90 degrees counter clockwise (assuming (0,0) is in the upper left corner
   * of the drawing).
 *
   * @return A copy of this floor plan with every coordinate rotated 90 degrees CCW around the lowest starting point in
   *         the original floor plan.
   */
  def rotate90CounterClockwise: FloorPlan[V, E] = {
    val degree = -90
    val rotatedRooms = roomAreas.map(area => (area, rotatePoint(area.start, degree), rotatePoint(area.stop, degree)))
    val smallestX = findMinX(rotatedRooms)
    val smallestY = findMinY(rotatedRooms)
    val xDifference = Math.abs(rLayout.minX - smallestX)
    val yDifference = Math.abs(rLayout.minY - smallestY)
    val xAdd = if (smallestX < rLayout.minX) xDifference else -xDifference
    val yAdd = if (smallestY < rLayout.minY) yDifference else -yDifference
    val newRooms = rotatedRooms.map(r => {
      val newStart = Point(r._2._1 + xAdd, r._2._2 + yAdd)
      val newStop = Point(r._3._1 + xAdd, r._3._2 + yAdd)

      // Change y values since the points now point from the upper right corner to the lower left.
      r._1.withNewCoordinates(Point(newStart.x, newStop.y), Point(newStop.x, newStart.y))
    })

    new FloorPlan(newRooms.toVector, rLayout)
  }

  /**
   * Changes the y axis of the drawing, causing it to be drawn upside down. The x axis remains as-is however, so rooms
   * on the left still stay left etc.
   */
  def flipYAxis: FloorPlan[V, E] = {

    // Change every y value to its negative
    val flippedRooms = roomAreas.map(area => (area, flipYPoint(area.start), flipYPoint(area.stop)))
    val minY = findMinY(flippedRooms)
    val yDifference = Math.abs(rLayout.minY - minY)

    val newRooms = flippedRooms.map(flipped => {
      val newStart = Point(flipped._2._1, flipped._2._2 + yDifference)
      val newStop = Point(flipped._3._1, flipped._3._2 + yDifference)

      // Switch start and stop y values to make the rectangle go from lower (x,y) to higher.
      flipped._1.withNewCoordinates(Point(newStart.x, newStop.y), Point(newStop.x, newStart.y))
    })

    new FloorPlan(newRooms.toVector, rLayout)
  }

  /**
   * Replaces areas in the floor plan with new ones.
 *
   * @param roomUpdates Tuples with areas currently in the floor plan, paired against the coordinates to update
   *                    them with.
   * @return A new floor plan where the input areas have their coordinates adjusted.
   */
  def updateRooms(roomUpdates: Vector[(RoomArea[V], ModifiedCoordinates)]): FloorPlan[V, E] = {
    val mapping = roomUpdates.toMap

    val updatedRooms = for {
      r <- roomAreas
      room = if (mapping.contains(r)) {
        val updatedCoordinates = mapping(r)
        r.withNewCoordinates(updatedCoordinates.start, updatedCoordinates.stop)
      } else {
        r
      }
    } yield room

    new FloorPlan(updatedRooms, rLayout)
  }

  /* Returns a tuple since points aren't allowed to be negative. */
  private def rotatePoint(p: Point, degrees: Int): (Int, Int) = {
    val xCenter = rLayout.minX
    val yCenter = rLayout.minY
    val angle = Math.toRadians(degrees)
    val xRotated = xCenter + (Math.cos(angle) * (p.x - xCenter)) - (Math.sin(angle) * (p.y - yCenter))
    val yRotated = yCenter + (Math.sin(angle) * (p.x - xCenter)) + (Math.cos(angle) * (p.y - yCenter))
    (Math.rint(xRotated).toInt, Math.rint(yRotated).toInt)
  }

  private def flipYPoint(p: Point): (Int, Int) = (p.x, -p.y)
  private def findMinX(rooms: Vector[(RoomArea[V], (Int, Int), (Int, Int))]): Int = rooms.map(e => Math.min(e._2._1, e._3._1)).min
  private def findMinY(rooms: Vector[(RoomArea[V], (Int, Int), (Int, Int))]): Int = rooms.map(e => Math.min(e._2._2, e._3._2)).min

  /* For every neighbor pair in the layout graph, this method computes an intersection object mapped to each room
   * of the vertices.
   */
  private def computeIntersections: Map[RoomArea[V], Vector[FloorIntersection[V]]] = {
    val vertexToRoom = roomAreas.filter(_.isRoom).map(r => r.originalRoom -> r).toMap
    val allGateNeighbors = gateNeighbors
    val allIntersections = new mutable.HashMap[RoomArea[V], Vector[FloorIntersection[V]]]()

    for (area <- roomAreas)
      allIntersections += area -> Vector()

    /* Compute intersections between areas represented by regular vertices. */
    for (vertex <- rLayout.graph.nodes) {
      val roomArea = vertexToRoom(vertex)
      val neighbors = GraphCommons.outerNeighbors[V, E](vertex, rLayout.graph)
      val vertexIntersections = new ListBuffer[FloorIntersection[V]]()

      for (n <- neighbors if !allGateNeighbors(vertex).contains(n)) {
        val neighborRoom = vertexToRoom(n)
        val intersection = Intersection(roomArea, neighborRoom)
        assume(intersection.intersects, "The vertices " + vertex + " and " + n + " has non-intersecting room areas " +
          "despite being adjacent in the graph that their rectangular dual was based upon.")
        assume(Geom.width(intersection.start.x, intersection.stop.x) >= minimumIntersectionLength || Geom.height(intersection.start.y, intersection.stop.y) >= minimumIntersectionLength,
          "The vertices " + vertex + " and " + n + " does not intersect in at least " + minimumIntersectionLength + " coordinates.")
        val dir = direction(roomArea, neighborRoom)
        vertexIntersections += FloorIntersection(dir, neighborRoom, Connection(intersection))
      }

      allIntersections += roomArea -> vertexIntersections.toVector
    }

    /* Compute intersections for every gate. For each neighbor of the gate, add an intersection to both the gate
     * and the neighbor (with reversed direction).
     */
    for (gate <- roomAreas.filter(_.isGate)) {
      val neighbors = gate.gateNeighbors
      addGateIntersection(gate, vertexToRoom(neighbors._1), allIntersections)
      addGateIntersection(gate, vertexToRoom(neighbors._2), allIntersections)
    }

    allIntersections.toMap
  }

  private def maxXY: (Int, Int) = {
    val mm = roomAreas.map(entry => (entry.stop.x, entry.stop.y))
    val maxX = mm.map(_._1).max
    val maxY = mm.map(_._2).max
    (maxX, maxY)
  }
  private def minXY: (Int, Int) = {
    val mm = roomAreas.map(entry => (entry.start.x, entry.start.y))
    val minX = mm.map(_._1).min
    val minY = mm.map(_._2).min
    (minX, minY)
  }

  private def direction(roomArea: RoomArea[V], neighborRoom: RoomArea[V]): Direction = {
    if (roomArea.start.y == neighborRoom.stop.y)
      North
    else if (roomArea.stop.y == neighborRoom.start.y)
      South
    else if (roomArea.start.x == neighborRoom.stop.x)
      West
    else if (roomArea.stop.x == neighborRoom.start.x)
      East
    else
      throw new Error("The drawn areas " + roomArea + " and " + neighborRoom + " did not intersect in a single coordinate interval.")
  }

  private def addGateIntersection(gate: RoomArea[V], neighbor: RoomArea[V], allIntersections: mutable.HashMap[RoomArea[V], Vector[FloorIntersection[V]]]) {
    val gateIntersection = Intersection(gate, neighbor)
    assume(gateIntersection.intersects, "The gate " + gate + " and its neighbor" + neighbor + " has non-intersecting room areas.")
    assume(Geom.width(gateIntersection.start.x, gateIntersection.stop.x) >= minimumIntersectionLength || Geom.height(gateIntersection.start.y, gateIntersection.stop.y) >= minimumIntersectionLength,
      "The gate " + gate + " and the vertex " + neighbor + " does not intersect in at least " + minimumIntersectionLength + " coordinates.")

    val dir = direction(gate, neighbor) // from the gates PoV
    val gateFloorIntersection = FloorIntersection(dir, neighbor, Connection(gateIntersection))
    val gateIntersections = allIntersections(gate)
    allIntersections += gate -> (gateFloorIntersection +: gateIntersections)

    var currentNeighborIntersections = allIntersections(neighbor)
    currentNeighborIntersections = FloorIntersection(dir.opposite, gate, Connection(gateIntersection)) +: currentNeighborIntersections
    allIntersections += neighbor -> currentNeighborIntersections
  }

  private def gateNeighbors: Map[V, Set[V]] = {
    val map = new mutable.HashMap[V, Set[V]]()
    for (room <- roomAreas.filter(_.isRoom))
      map += room.originalRoom -> Set()

    for (gate <- roomAreas.filter(_.isGate)) {
      val n1 = gate.gateNeighbors._1
      val n2 = gate.gateNeighbors._2

      val n1Set = map.get(n1).getOrElse(Set())
      val n2Set = map.get(n2).getOrElse(Set())

      map += n1 -> (n1Set + n2)
      map += n2 -> (n2Set + n1)
    }
    map.toMap
  }

  // Throws exceptions if two areas intersect by more than their borders
  private def checkForIntersectionOverlap() {
    var unprocessedRooms = roomAreas
    while (!unprocessedRooms.isEmpty) {
      val room = unprocessedRooms.head
      unprocessedRooms = unprocessedRooms.drop(1)

      val remainingRooms = unprocessedRooms.iterator
      while (remainingRooms.hasNext) {
        val otherRoom = remainingRooms.next()
        if (otherRoom != room) {
          val intersection = Intersection(room, otherRoom)

          if (intersection.intersects) {
            val start = intersection.start
            val stop = intersection.stop

            if (stop.x - start.x > 0 && stop.y - start.y > 0)
              throw new Error("Room overlap found between rectangles " + room + " and " + otherRoom + ".")
          }
        }
      }
    }
  }
}

/**
 * Floor plan constructor object available to the user.
 */
object FloorPlan {

  /**
   * Computes a floor plan from a rectangular layout.
 *
   * @param layout Layout to base floor plan on.
   * @return The constructed floor plan.
   */
  def apply[V <: MapArea, EType[X] <: UnDiEdge[X]]
    (layout: RectangularLayout[V, EType]): FloorPlan[V, EType] = new FloorPlan(computeRooms(layout), layout)

  /*
   * Instantiates a new room container for each rectangle in the original layout. The coordinates of each room are
   * scaled by first computing the new size of the total drawing (the number of unique stop coordinates along each
   * axis times the minimum axis increase), and then scaling every individual start/stop coordinate to fit the new
   * size.
   */
  private def computeRooms[V <: MapArea, EType[X] <: UnDiEdge[X]](layout: RectangularLayout[V, EType]): Vector[RoomArea[V]] = {
    val newRooms = for {
      vertexAndRectangle <- layout.rectangles
      vertex = vertexAndRectangle._1
      rect = vertexAndRectangle._2
    } yield RoomArea(vertex, Point(rect.startX, rect.startY), Point(rect.stopX, rect.stopY))

    val newGates = for {
      gate <- layout.gates
    } yield RoomArea(gate.from, gate.to, Point(gate.startX, gate.startY), Point(gate.stopX, gate.stopY))

    newRooms.toVector ++ newGates
  }
}