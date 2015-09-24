package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.{FloorIntersection, RoomArea}
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.{Point, Direction, Geom}
import scala.collection.mutable.ListBuffer
import net.cyndeline.scalarlib.rldrawing.util.Direction.Direction
import scala.collection.mutable

/**
 * Reduces a single rectangle in a floor plan down to its target size, if doing so is possible without breaking
 * adjacency constraints. Note that this class only reduces areas that are too large, ensuring that every area meets
 * its minimum size before reducing them should be handled elsewhere.
 *
 * A side of a rectangle R can only be moved inwards if it does not connect to any neighbors of R.
 * Thus every such segment is a candidate for reducing the rectangles size. Since every area represented by a
 * vertex has a minimum of one neighbor (unless specified as an exception by the user), at most three segments can
 * be moved per rectangle.
 *
 * Algorithm description:
 *
 *  1. Every segment available for moving is computed
 *  1. If there are neighbors connecting to the segments on the opposite axis, the furthest away coordinates of the
 *  intersections closest to the segments current position on both those axises are computed. The segment can never
 *  move past that, as doing so would break adjacency. A one coordinate margin must also be kept between the segment
 *  and the intersection to avoid diagonal holes between walls.
 *  1. While the target size has not been reached, the segments still able to move without hitting an adjacency
 *  constraint or the maximum aspect ratio are moved 1 step. If this involves more than one segment, the shortest
 *  one is prioritized to reach a 1:1 aspect ratio if possible.
 *  1. If the target size is met, or if every segment hits an adjacency constraint, or if the only segments capable
 *  to move cannot do so without hitting the maximum aspect ratio, Exit.
 *
 * @param maxAspectRatio The highest allowed ratio discrepancy between sides in rectangles. Setting this to 1.0 will
 *                       limit the scaling to areas that exceed this ratio, and only scale them until they meet it
 *                       for the first time.
 * @param minimumSideLength The lowest amount of coordinates that should exist along each axis of the initial room.
 */
class RectangleScaler(maxAspectRatio: Double, minimumSideLength: Int) {
  require(maxAspectRatio >= 1, "The maximum aspect ratio must be set at 1 (1:1) or higher.")
  require(minimumSideLength >= 1, "The room must be at least 1 coordinate large in both directions.")

  // The number of extra coordinates required in the length of each intersection to avoid diagonal gaps
  private val intersectionMargin = 2

  /**
   * @param area An area that has a size equal or higher than its target size.
   * @param targetSize The target size of the area.
   * @param intersections Intervals on the area that connects to neighbors that the area must remain adjacent to.
   * @return The new coordinates for the room if any modification occurred, or None if no modifications were possible
   *         or the room was already optimal.
   */
  def scaleDown[V](area: RoomArea[V],
                   targetSize: Int,
                   intersections: Vector[FloorIntersection[V]]): Option[ModifiedCoordinates] = {
    scaleDownEntry(area, targetSize, intersections, Vector())
  }

  /**
   * @param area An area that has a size equal or higher than its target size.
   * @param targetSize The target size of the area.
   * @param intersections Intervals on the area that connects to neighbors that the area must remain adjacent to.
   * @param outerCoordinates If the room is an outer area containing the door into the layout, this vector will contain
   *                         which sides of the area (at least 1) that face outwards, and what the maximum (or minimum
   *                         depending on the side) coordinate the area must be equal or higher (lower) than in order
   *                         to remain as the area that is furthest out in that direction.
   * @return The new coordinates for the room if any modification occurred, or None if no modifications were possible
   *         or the room was already optimal.
   */
  def scaleDownEntry[V](area: RoomArea[V],
                        targetSize: Int,
                        intersections: Vector[FloorIntersection[V]],
                        outerCoordinates: Vector[(Direction, Int)]): Option[ModifiedCoordinates] = {
    val sidesThatCanBeMoved = Vector(North, South, West, East) diff intersections.map(_.direction).distinct
    val areaRep = new AreaRep(area, maxAspectRatio)
    val outerCoordMap: Map[Direction, Int] = outerCoordinates.toMap
    val segments: Vector[SingleSegment[V]] = computeSegments(area, sidesThatCanBeMoved, intersections, outerCoordMap)

    for (s <- segments)
      s.area = areaRep

    var reductionPerformed = false

    if (segments.isEmpty) {
      None
    } else {
      var remainingSegments = Map[Direction, SingleSegment[V]]() ++ segments.map(s => s.d -> s)
      remainingSegments = remainingSegments.filter(kv => kv._2.canMove)

      while (!remainingSegments.isEmpty && areaRep.area > targetSize) {

        /* To alternate between which side that gets reduced (assuming there are several), the side vector is reversed
         * for every other reduction.
         */
        val shortestSides = if (areaRep.area % 2 == 0) areaRep.shortestSide else areaRep.shortestSide.reverse
        var side = shortestSides.find(remainingSegments.contains)
        val segmentAndSide = findSegmentToMove(side, areaRep, remainingSegments)
        side = segmentAndSide._1
        val segment = segmentAndSide._2

        /* If the shortest sides cannot be moved, and the aspect ratio cannot increase, we're done. */
        if (segment.isEmpty) {
          remainingSegments = remainingSegments.empty

        } else if (areaRep.areaAfterReduction(side.get) >= targetSize) {
          segment.get.reduceBy1() // Moves the segment inwards by 1
          reductionPerformed = true

          if (!segment.get.canMove)
            remainingSegments = remainingSegments - side.get

        } else {

          // If there is a segment, but moving it puts the size below target, there's no use moving it any further
          remainingSegments = remainingSegments - side.get
        }
      }
    }

    if (reductionPerformed)
      Some(ModifiedCoordinates(Point(areaRep.left, areaRep.top), Point(areaRep.right, areaRep.bottom)))
    else
      None
  }

  /**
   * @param stop The lowest/highest value the segment is allowed to assume.
   */
  private class SingleSegment[V](val d: Direction, stop: Int) {

    def canMove: Boolean = {
      val topCoord = Math.max(area.coordinate(d), area.coordinate(d.opposite))
      val bottomCoord = Math.min(area.coordinate(d), area.coordinate(d.opposite))
      val minimum = Geom.width(bottomCoord, topCoord) > minimumSideLength
      val notStopped = d match {
        case North | West => value < stop
        case South | East => value > stop
      }

      minimum && notStopped
    }
    def value = d match {
      case West => area.left
      case East => area.right
      case North => area.top
      case South => area.bottom
    }
    def reduceBy1() = d match {
      case West => area.reduceLeft()
      case East => area.reduceRight()
      case North => area.reduceTop()
      case South => area.reduceBottom()
    }

    var area: AreaRep[V] = null

    override def toString: String = "Segment " + d + ", stops: " + stop
  }

  private class AreaRep[V](a: RoomArea[V], aspectRatio: Double) {
    private var startX = a.start.x
    private var startY = a.start.y
    private var stopX = a.stop.x
    private var stopY = a.stop.y

    def area = Geom.area(startX, stopX, startY, stopY)

    def shortestSide: Vector[Direction] = {
      if (stopX - startX > stopY - startY)
        Vector(West, East)
      else if (stopX - startX < stopY - startY)
        Vector(North, South)
      else
        Vector()
    }

    def canIncreaseRatio: Boolean = {
      val width = stopX - startX
      val height = stopY - startY
      val ratio = Math.max(width / height, height / width)
      ratio < aspectRatio
    }

    def left: Int = startX
    def right: Int = stopX
    def top: Int = startY
    def bottom: Int = stopY

    def coordinate(d: Direction): Int = d match {
      case North => top
      case West => left
      case East => right
      case South => bottom
    }

    def reduceLeft() { startX += 1 }
    def reduceRight() { stopX -= 1 }
    def reduceTop() { startY += 1 }
    def reduceBottom() { stopY -= 1 }

    def areaAfterReduction(d: Direction): Int = {
      var startX = left
      var startY = top
      var stopX = right
      var stopY = bottom
      d match {
        case North => startY += 1
        case West => startX += 1
        case East => stopX -= 1
        case South => stopY -= 1
      }
      val aa = Geom.area(startX, stopX, startY, stopY)
      aa
    }

  }

  private def computeSegments[V](area: RoomArea[V],
                                 sidesThatCanBeMoved: Vector[Direction],
                                 intersections: Vector[FloorIntersection[V]],
                                 outerCoordMap: Map[Direction, Int]): Vector[SingleSegment[V]] = {
    for {
      side <- sidesThatCanBeMoved
      ps = side.parallel
      limitsOnOrthogonalSide = MinimumSegmentCoordinates.minCoordinatesForSide(ps == North || ps == South, intersections, area)

      // Increased by 1 to keep a 1-coordinate margin, preventing diagonal openings in walls
      stop = cIncreasedByAmount(sideLimit(side, limitsOnOrthogonalSide), side, intersectionMargin)

      /* If an outer coordinate limit is defined for the entry room, it will always occur before the end of any inner
       * intersection interval, even if the neighbor that lies closest to the limit is connected to the room. */
      stopCoordinate = if (outerCoordMap.contains(side)) outerCoordMap(side) else stop
      segment = new SingleSegment[V](side, stopCoordinate)
    } yield segment
  }

  private def findSegmentToMove[V](side: Option[Direction],
                                   areaRep: AreaRep[V],
                                   remainingSegments: Map[Direction, SingleSegment[V]]): (Option[Direction], Option[SingleSegment[V]]) = {
    if (side.isEmpty && areaRep.canIncreaseRatio) {

      /* If the shortest sides directions doesn't contain a segment that can move, the remaining segments in
       * the map belong to the longer side. Doesn't matter which one is chosen. Selecting a longer side to move
       * increases the aspect ratio however.
       */
      val newSide = if (areaRep.area % 2 == 0) Some(remainingSegments.head._1) else Some(remainingSegments.last._1)
      (newSide, Some(remainingSegments(newSide.get)))

    } else if (side.isDefined) {
      (side, Some(remainingSegments(side.get)))
    } else {
      (side, None)
    }
  }

  /**
   * @param d The side on which the coordinate represents a segment.
   */
  private def cIncreasedByAmount(c: Int, d: Direction, amount: Int): Int = d match {
    case North | West => c - amount
    case South | East => c + amount
  }

  private def sideLimit(d: Direction, cs: (Int, Int)) = d match {
    case North | West => cs._1
    case South | East => cs._2
  }
}
