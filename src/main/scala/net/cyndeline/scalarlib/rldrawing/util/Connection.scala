package net.cyndeline.scalarlib.rldrawing.util

import net.cyndeline.rlcommon.util.{Intersection, Point}

/**
 * Computes coordinates where two areas intersects each other (examples: Two adjacent room, or a room and a connected
 * corridor).
 */
class Connection private (val start: Point, val stop: Point) {
  require(start.x == stop.x || start.y == stop.y, "Cannot create area connection that spans more than 1 coordinate on both axises.")

  /*** @return Iterates over every coordinate in the connection starting from the top/left. */
  def iterator: Iterator[Point] = if (start.x == stop.x) {
    (for (i <- start.y to stop.y) yield Point(start.x, i)).iterator
  } else {
    (for (i <- start.x to stop.x) yield Point(i, start.y)).iterator
  }

  override def equals(other: Any): Boolean = other match {
    case c: Connection => start == c.start && stop == c.stop
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.##

  override def toString: String = "Connection[" + start + " -> " + stop + "]"

}

/**
 * Factory object for instantiating connections.
 */
object Connection {

  /**
   * @param start A coordinate.
   * @param stop A coordinate that shares one axis with start.
   * @return A connection iterating over all coordinates between start and stop.
   */
  def apply(start: Point, stop: Point): Connection = {
    val upperLeft = Point(Math.min(start.x, stop.x), Math.min(start.y, stop.y))
    val lowerRight =Point(Math.max(start.x, stop.x), Math.max(start.y, stop.y))
    new Connection(upperLeft, lowerRight)
  }

  /**
   * @param startAndStop Two coordinates sharing an axis.
   * @return A connection iterating over all coordinates between start and stop.
   */
  def apply(startAndStop: (Point, Point)): Connection = apply(startAndStop._1, startAndStop._2)

  /**
   * @param intersection An intersection that covers a single coordinate along one of its axises.
   * @return A connection iterating over all coordinates between the start and stop coordinate of the intersection.
   */
  def apply(intersection: Intersection): Connection = apply(intersection.start, intersection.stop)

}
