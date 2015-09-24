package net.cyndeline.scalarlib.util.datastructure.mutable

/**
 * An integer interval associated with some data. Used in interval trees.
 */
class Interval[T](val start: Long, val stop: Long, val data: Option[T]) extends Comparable[Interval[T]] {
  require(start < stop, "Intervals must go from a lower value to a higher.")

  def this(start: Long, stop: Long) = this(start, stop, None)

  def this(start: Long, stop: Long, data: T) = this(start, stop, Option(data))

  /**
   * @param point Point to check for membership in this interval.
   * @return True if this interval includes the point (inclusive with both start and stop), otherwise false.
   */
  def contains(point: Long): Boolean = point >= start && point <= stop

  /**
   * @param other Interval to check for intersection with this one.
   * @return True if the other interval shares at least one coordinate with this one (inclusive), otherwise false.
   */
  def intersects(other: Interval[T]): Boolean = other.stop >= start && other.start <= stop

  /**
   * @return -1 if this intervals start coordinate is less than the others, 1 if it is greater. If both intervals
   *         share the same start coordinate: -1 if this intervals stop coordinate is less than the others, 1 if
   *         it is higher. 0 if both intervals have the same coordinates.
   */
  def compareTo(other: Interval[T]): Int = if (start < other.start) -1
    else if (start > other.start) 1
    else if (stop < other.stop) -1
    else if (stop > other.stop) 1
    else 0

  override def equals(other: Any): Boolean = other match {
    case i: Interval[T] => i.start == start && i.stop == stop && i.data == data
    case _ => false
  }

  override val hashCode: Int = start.## ^ stop.## ^ data.##

  override def toString: String = "[" + start + ", " + stop + "]"
}
