package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.{FloorIntersection, RoomArea}

/**
 * Given an area with two sides (top/bottom or left/right) and a list of intervals, this object computes the coordinates
 * on the parallel interval that represents the closest inner coordinate on the interval closest to each side.
 */
object MinimumSegmentCoordinates {

  /**
   * @param side True if the segment with intersections to check lies at top/bottom, false if left/right.
   * @return The minimum coordinates across both sides of the parallel direction that the area must stay connected to,
   *         or the start/stop coordinates of the area if neither side has intersections on it.
   */
  def minCoordinatesForSide[V](side: Boolean,
                               intersections: Vector[FloorIntersection[V]],
                               area: RoomArea[V]): (Int, Int) = {
    val dirA = if (side) North else West
    val dirB = dirA.opposite
    val coordinates = Vector(minCoordinatesForDirection(dirA, intersections), minCoordinatesForDirection(dirB, intersections))
      .filter(_.isDefined)
      .map(_.get)

    if (coordinates.isEmpty) {
      val pDirA = if (dirA == North) West else North
      val pDirB = if (dirB == East) South else East
      val lower = segmentCoordinate(pDirA, area)
      val upper = segmentCoordinate(pDirB, area)

      (upper, lower)

    } else {
      val lower = coordinates.map(_._1).min
      val upper = coordinates.map(_._2).max

      (lower, upper)
    }
  }

  /** Given a directions, computes the minimum left and right (top/bottom) intersection coordinate that the area being
    * reduced must stay connected to. The first value is the left (top) coordinate that the right (bottom) coordinate
    * of the area must stay inside, and the second is the right (bottom) coordinate that the left (top) coordinate
    * of the area must stay inside.
    *
    * None if no intersections exist in that direction.
    */
  private def minCoordinatesForDirection[V](direction: Direction, intersections: Vector[FloorIntersection[V]]): Option[(Int, Int)] = {
    val mappedIntersections = intersections
      .filter(_.direction == direction)
      .map(i => i.direction match {
      case North | South => i.connection.start.x -> i.connection.stop.x
      case West | East => i.connection.start.y -> i.connection.stop.y
    })

    if (mappedIntersections.isEmpty) {
      None
    } else {

      // We want to find the lowest upper value to use when limiting reduction from below, and the highest lower value
      // to use when limiting reduction from above.
      val lowestUpper = mappedIntersections.minBy(_._2)._2
      val highestLower = mappedIntersections.maxBy(_._1)._1

      Some((lowestUpper, highestLower))
    }
  }

  private def segmentCoordinate[V](d: Direction, area: RoomArea[V]): Int = d match {
    case North => area.start.y
    case West => area.start.x
    case South => area.stop.y
    case East => area.stop.x
  }

}
