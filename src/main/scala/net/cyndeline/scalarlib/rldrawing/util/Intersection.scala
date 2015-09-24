package net.cyndeline.scalarlib.rldrawing.util

import net.cyndeline.scalarlib.rldrawing.common.RectangleCoordinates

/**
 * Computes the intersection of two rectangle points. If no intersection exists, an exception is thrown when
 * attempting to retrieve intersection coordinates.
 *
 * @param r1Start Starting point for the first rectangle.
 * @param r1Stop Ending point for the first rectangle (inclusive).
 * @param r2Start Starting point for the second rectangle.
 * @param r2Stop Ending point for the second rectangle (inclusive).
 */
class Intersection private (r1Start: Point, r1Stop: Point, r2Start: Point, r2Stop: Point) extends RectangleCoordinates {
  require(r1Start.x <= r1Stop.x && r2Start.x <= r2Stop.x, "Starting x of a rectangle must be <= to stop.")
  require(r1Start.y <= r1Stop.y && r2Start.y <= r2Stop.y, "Starting y of a rectangle must be <= to stop.")

  def width: Int = Geom.width(start.x, stop.x)
  def height: Int = Geom.height(start.y, stop.y)

  private val xIntersect: Int = {
    val x11 = r1Start.x
    val x12 = r1Stop.x
    val x21 = r2Start.x
    val x22 = r2Stop.x

    Math.min(x12,x22) - Math.max(x11,x21)
  }

  private val yIntersect: Int = {
    val y11 = r1Start.y
    val y12 = r1Stop.y
    val y21 = r2Start.y
    val y22 = r2Stop.y

    Math.min(y12, y22) - Math.max(y11, y21)
  }

  private val startValue: Option[Point] = computeStart

  // Relies on startValue already being computed
  private val stopValue: Option[Point] = computeStop

  /** @return True if both rectangles overlap or shares an edge. Otherwise false. */
  def intersects: Boolean = xIntersect >= 0 && yIntersect >= 0

  /** @return The start point of the intersecting rectangle, if an intersection exists. Start.x/y <= stop.x/y.*/
  override def start: Point = {
    require(intersects, "No intersection available.")
    startValue.get
  }

  /** @return The stop point of the intersecting rectangle, if an intersection exists. Start.x/y <= stop.x/y.*/
  override def stop: Point = {
    require(intersects, "No intersection available.")
    stopValue.get
  }

  private def computeStart: Option[Point] = {
    if (intersects) {
      val x = if (r1Start.x > r2Start.x)
        r1Start.x
      else
        r2Start.x

      val y = if (r1Start.y > r2Start.y)
        r1Start.y
      else
        r2Start.y

      Some(Point(x, y))
    } else {
      None
    }
  }

  private def computeStop: Option[Point] = {
    if (intersects) {
      val xStop = Math.min(r1Stop.x, r2Stop.x)
      val yStop = Math.min(r1Stop.y, r2Stop.y)
      Some(Point(xStop, yStop))

    } else {
      None
    }
  }
}

/**
 * Factory object available to users.
 */
object Intersection {

  /**
   * Computes the intersection between two rectangular coordinate objects.
   * @param r1 A rectangle.
   * @param r2 A rectangle.
   * @return The intersection between the rectangles start and stop coordinates.
   */
  def apply(r1: RectangleCoordinates, r2: RectangleCoordinates) = new Intersection(r1.start, r1.stop, r2.start, r2.stop)

  /**
   * Computes an intersection between the start and stop coordinates of a rectangle.
   * @param r1Start Starting point for the first rectangle.
   * @param r1Stop Ending point for the first rectangle (inclusive).
   * @param r2Start Starting point for the second rectangle.
   * @param r2Stop Ending point for the second rectangle (inclusive).
   * @return The intersection between the start and stop coordinates.
   */
  def apply(r1Start: Point, r1Stop: Point, r2Start: Point, r2Stop: Point) = new Intersection(r1Start, r1Stop, r2Start, r2Stop)

  /**
   * @param aStartX Start x coordinate of the first rectangle.
   * @param aStartY Start y coordinate of the first rectangle.
   * @param aStopX Stop x coordinate of the first rectangle.
   * @param aStopY Stop y coordinate of the first rectangle.
   * @param bStartX Start x coordinate of the second rectangle.
   * @param bStartY Start y coordinate of the second rectangle.
   * @param bStopX Stop x coordinate of the second rectangle.
   * @param bStopY Stop y coordinate of the second rectangle.
   * @return The intersection between the start and stop coordinates.
   */
  def apply(aStartX: Int, aStartY: Int, aStopX: Int, aStopY: Int, bStartX: Int, bStartY: Int, bStopX: Int, bStopY: Int)
    = new Intersection(Point(aStartX, aStartY), Point(aStopX, aStopY), Point(bStartX, bStartY), Point(bStopX, bStopY))

}
