package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Rectangle
import net.cyndeline.scalarlib.rldrawing.util.RectangleArea

/**
 * A rectangle with orthogonal sides defined by its starting and stopping coordinates.
 *
 * @constructor Constructs a new rectangular area.
 * @param startPoint Starting coordinates for the upper left corner of the rectangle, must be less than or equal
 *                   to the stop coordinates.
 * @param stopPoint Stop coordinates for the lower right corner of the rectangle, must be higher than or equal
 *                  to the start coordinates.
 */
case class AdjustableRectangle(startPoint: Point, stopPoint: Point) extends RectangleArea(startPoint, stopPoint) {

  /**
   * @param side The side to check length of.
   * @return The number of coordinates that makes up the specified side of the area rectangle.
   */
  def lengthOfSide(side: Direction): Int = side match {
    case North => stop.x - start.x + 1
    case South => lengthOfSide(North)
    case West => stop.y - start.y + 1
    case East => lengthOfSide(West)
  }

  /**
   * Returns the start and stop coordinates that are modified when traversing a side of the rectangle.
   * Example: A rectangle starting at (2,3) and ending at (7,9) would have coordinates (3, 9) along the west
   * and east side, and (2, 7) along the north and south side.
    *
    * @param side Which side of the rectangle to retrieve coordinates for.
   * @return An integer tuple with the coordinate start/stop pair along the axis that is modified when traversing
   *         the specified size.
   */
  def coordinatesOnSide(side: Direction): (Int, Int) = side match {
    case North => (start.x, stop.x)
    case South => coordinatesOnSide(North)
    case West => (start.y, stop.y)
    case East => coordinatesOnSide(West)
  }

  /**
   * Creates a new rectangle by modifying the coordinates of this object by a specified amount.
    *
    * @param dir Direction to "move" the rectangle in.
   * @param amount Amount of coordinates to move the rectangle.
   * @return A copy of this rectangle with new coordinates.
   */
  def adjustCoordinates(dir: Direction, amount: Int): AdjustableRectangle = {
    val newStart = dir match {
      case North => Point(start.x, start.y - amount)
      case South => Point(start.x, start.y + amount)
      case West => Point(start.x - amount, start.y)
      case East => Point(start.x + amount, start.y)
    }
    val newStop = dir match {
      case North => Point(stop.x, stop.y - amount)
      case South => Point(stop.x, stop.y + amount)
      case West => Point(stop.x - amount, stop.y)
      case East => Point(stop.x + amount, stop.y)
    }

    AdjustableRectangle(newStart, newStop)
  }

  /**
   * Computes start/stop coordinates for a specified side of the area.
    *
    * @param dir Side of the area to compute coordinates for.
   * @return Start/stop coordinate of the specified side. Ex: For an area between (1,2) and (3,4), the east side
   *         has coordinates (3,2) and (3,4).
   */
  def coordinatesForSide(dir: Direction): (Point, Point) = {
    val borderStart = dir match {
      case North => start
      case South => Point(start.x, stop.y)
      case West => start
      case East => Point(stop.x, start.y)
    }

    val borderStop = dir match {
      case North => Point(stop.x, start.y)
      case South => stop
      case West => Point(start.x, stop.y)
      case East => stop
    }

    (borderStart, borderStop)
  }

  /**
    * Checks if this rectangle is adjacent (but not overlapping) to another rectangle in a specified direction.
    *
    * @param other Rectangle to check adjacency to.
    * @param direction Side of this area that the other rectangle should be positioned in to be considered adjacent.
    * @return True if the other rectangle lies 1 coordinate to the side of this area (in the specified direction) and
    *         also shares some coordinate(s) on the interval of the specified side, otherwise false.
    */
  def adjacentTo(other: AdjustableRectangle, direction: Direction): Boolean = {

    /* The easiest way to check is to first make sure both areas doesn't overlap (if so they don't qualify as
     * neighbors). If moving the other rectangle 1 coordinate in the opposite specified direction then makes them
     * overlap, it must have been a neighbor before the move.
     */
    if (this.overlaps(other)) return false

    /* If the other rectangle lies at coordinate 0 on any axis and the direction is the opposite of
     * south/east, it cannot be a neighbor since it cannot be adjusted past 0 on the axis.
     */
    if (direction == South && other.start.y == 0) return false
    if (direction == East && other.start.x == 0) return false

    val adjusted = other.adjustCoordinates(direction.opposite, 1)
    this.overlaps(adjusted)
  }

  /**
   * Modifies the coordinates on some side to move them closer to the coordinates on the opposite side.
   * Example: Reducing from the north side will have the start y coordinate increased, reducing from the south side
   * will have the stop.y coordinate reduced.
   *
   * @param side Which side that should "move" towards the other.
   * @return A copy of the rectangle with the new coordinate set.
   */
  def reduceFromSide(side: Direction, amount: Int): AdjustableRectangle = {
    if (amount < 0) throw new Error("Cannot reduce rectangles by a negative amount.")
    if (amount == 0) throw new Error("Attempted to reduce area " + this + " by 0.")

    side match {
      case North => AdjustableRectangle(Point(start.x, start.y + amount), stop)
      case South => AdjustableRectangle(start, Point(stop.x, stop.y - amount))
      case West => AdjustableRectangle(Point(start.x + amount, start.y), stop)
      case East => AdjustableRectangle(start, Point(stop.x - amount, stop.y))
    }
  }

  /**
    * Computes a new sub-rectangle that represents the intersecting spaces between this rectangle and another.
    *
    * @param other Other rectangle to intersect with this one.
    * @return A new rectangular area showing the intersecting coordinates of the two rectangles, or None if they
    *         do not intersect. Rectangles that touches at the border counts as intersecting.
    */
  def intersection(other: AdjustableRectangle): Option[AdjustableRectangle] = {
    val i = this.intersection(other.asInstanceOf[RectangleArea])
    if (i.isDefined)
      Option(AdjustableRectangle(i.get.start, i.get.stop))
    else
      None
  }

  override def toString: String = "Rectangle[" + start + ", " + stop + "]"

  override def equals(other: Any) = other match {
    case rc: Rectangle => rc.start == start && rc.stop == stop
    case _ => false
  }

  override def hashCode: Int = start.## ^ stop.##

}
