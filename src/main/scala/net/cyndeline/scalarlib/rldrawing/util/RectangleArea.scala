package net.cyndeline.scalarlib.rldrawing.util

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Rectangle

/**
  * A rectangle with inclusive start and stop coordinates.
  */
class RectangleArea(val start: Point, val stop: Point) extends Rectangle {
  if (start.x > stop.x || start.y > stop.y)
    throw new IllegalArgumentException("The start coordinates for a rectangular area must be higher than the stop coordinates (start = " + start + " and stop = " + stop + ")")

  /**
    * Checks if a point lies inside the rectangle.
    *
    * @param p An x/y coordinate pair.
    * @return True if the point lies inside the rectangle, otherwise false. A point on the edge of the rectangle
    *         (i.e (1,1) in a rectangle from (1,1) to (3,3)) is considered to be inside.
    */
  def pointInside(p: Point): Boolean = p.x >= start.x && p.x <= stop.x && p.y >= start.y && p.y <= stop.y

  /**
    * Checks if this rectangle overlaps with another.
    *
    * @param other Other rectangle to check overlap against this one with.
    * @return True if both rectangles overlap, otherwise false. Two rectangles sharing a single coordinate is
    *         considered to be overlapping.
    */
  def overlaps(other: RectangleArea): Boolean = {
    start.x <= other.stop.x && stop.x >= other.start.x && start.y <= other.stop.y && stop.y >= other.start.y
  }

  /**
    * Computes a new sub-rectangle that represents the intersecting spaces between this rectangle and another.
    *
    * @param other Other rectangle to intersect with this one.
    * @return A new rectangular area showing the intersecting coordinates of the two rectangles, or None if they
    *         do not intersect. Rectangles that touches at the border counts as intersecting.
    */
  def intersection(other: RectangleArea): Option[RectangleArea] = {
    val startX = Math.max(start.x, other.start.x)
    val startY = Math.max(start.y, other.start.y)
    val stopX = Math.min(stop.x, other.stop.x)
    val stopY = Math.min(stop.y, other.stop.y)

    if (startX > stopX || startY > stopY) // Degenerate rectangle, no intersections
      None
    else
      Option(RectangleArea(Point(startX, startY), Point(stopX, stopY)))
  }

  override def equals(other: Any): Boolean = other match {
    case r: RectangleArea => start == r.start && stop == r.stop
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.##
}

/**
  * factory object.
  */
object RectangleArea {
  def apply(start: Point, stop: Point) = new RectangleArea(start, stop)
}
