package net.cyndeline.scalarlib.util.datastructure.mutable

import scala.collection.mutable.ListBuffer

/**
 * Maps intervals to objects, and can be used to query which intervals a target interval intersects.
 *
 * @tparam T Data type associated with each interval.
 */
class IntervalTree[T](initialIntervals: Vector[Interval[T]]) {
  private var head: IntervalNode[T] = new IntervalNode[T](initialIntervals)
  private val allIntervals = new ListBuffer[Interval[T]]() ++= initialIntervals
  private var inSynch: Boolean = true // Set to false whenever the tree must be rebuilt before returning query
  private var currentSize = allIntervals.size

  def size: Int = currentSize

  /**
   * Returns intersection for a single point. Rebuilds the tree if not in synch.
   * @param p Point to check intersecting intervals for.
   * @return A list of every interval in the tree that shares a coordinate with the given point.
   */
  def intervalsWithPoint(p: Long): Vector[Interval[T]] = {
    build()
    head.intervalsWithPoint(p)
  }

  /**
   * Returns intersection for an interval. Rebuilds the tree if not in synch.
   * @param interval Interval object to check intersection for.
   * @return A list of every interval in the tree that shares a coordinate with the given interval.
   */
  def intervalsIntersectingWith(interval: Interval[T]): Vector[Interval[T]] = {
    intervalsIntersectingWith(interval.start, interval.stop)
  }

  /**
   * Returns intersection for an interval. Rebuilds the tree if not in synch.
   * @param start Start coordinate of an interval.
   * @param stop Stop coordinate of an interval.
   * @return A list of every interval in the tree that shares a coordinate with the given interval.
   */
  def intervalsIntersectingWith(start: Long, stop: Long): Vector[Interval[T]] = {
    build()
    head.intervalsIntersectingWith(new Interval(start, stop))
  }

  /**
   * Adds an interval to the tree. Tree must be rebuilt before changes take effect.
   * @param start Start coordinate of the interval.
   * @param stop Stop coordinate of the interval.
   * @param data Data associated with the interval.
   * @return The interval that was added to the tree.
   */
  def addInterval(start: Long, stop: Long, data: T): Interval[T] = {
    inSynch = false
    val in = new Interval(start, stop, data)
    allIntervals += in
    in
  }

  /**
   * Removes an interval from the tree. Tree must be rebuilt before changes take effect.
   * @param i Interval to remove.
   */
  def removeInterval(i: Interval[T]) {
    if (allIntervals.contains(i)) {
      inSynch = false
      allIntervals -= i
    } else {
      throw new NoSuchElementException("Could not remove interval " + i + ", not present in interval tree.")
    }
  }

  /**
   * Updates the tree and its size with all added intervals since the last build.
   */
  def build(): Unit = {
    if (!inSynch) {
      head = new IntervalNode[T](allIntervals.toVector)
      inSynch = true
      currentSize = allIntervals.size
    }
  }

}
