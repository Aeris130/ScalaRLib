package net.cyndeline.scalarlib.util.datastructure.mutable

import scala.collection.mutable.ListBuffer
import java.util

/**
 * A single node in an interval tree.
 */
class IntervalNode[T](intervalList: Vector[Interval[T]]) {
  private val intervals = new util.TreeMap[Interval[T], ListBuffer[Interval[T]]]()
  private val endpoints: util.SortedSet[Long] = new util.TreeSet[Long]()

  private var centerValue: Long = 0
  private var leftNode: IntervalNode[T] = null
  private var rightNode: IntervalNode[T] = null

  /* Setup */

  private def setup(): Unit = {
    for (interval <- intervalList) {
      endpoints.add(interval.start)
      endpoints.add(interval.stop)
    }

    val median = computeMedian(endpoints)
    center = median

    val leftIntervals = new ListBuffer[Interval[T]]()
    val rightIntervals = new ListBuffer[Interval[T]]()
    val allIntervals = intervalList.iterator

    while (allIntervals.hasNext) {
      val interval = allIntervals.next()

      if (interval.stop < median) {
        leftIntervals += interval
      } else if (interval.start > median) {
        rightIntervals += interval
      } else {

        // Add interval to this node
        val currentIntervals = Option(intervals.get(interval)).getOrElse {
          val newList = new ListBuffer[Interval[T]]()
          intervals.put(interval, newList)
          newList
        }

        currentIntervals += interval
      }
    }

    if (!leftIntervals.isEmpty)
      left = new IntervalNode(leftIntervals.toVector)
    if (!rightIntervals.isEmpty)
      left = new IntervalNode(rightIntervals.toVector)
  }

  setup()

  /* End setup */

  def center: Long = centerValue
  def center_=(newCenter: Long): Unit = centerValue = newCenter
  def left: IntervalNode[T] = leftNode
  def right: IntervalNode[T] = rightNode
  def left_=(newLeft: IntervalNode[T]): Unit = leftNode = newLeft
  def right_=(newRight: IntervalNode[T]): Unit = rightNode = newRight

  /**
   * @param p A point on an interval.
   * @return Every interval in this node and its children that contains the point p.
   */
  def intervalsWithPoint(p: Long): Vector[Interval[T]] = {
    val result = new ListBuffer[Interval[T]]()
    val intervalEntries = intervals.entrySet().iterator()
    var done = false
    while (!done && intervalEntries.hasNext) {
      val entry = intervalEntries.next()

      if (entry.getKey.contains(p)) {
        for (interval <- entry.getValue)
          result += interval
      } else if (entry.getKey.start > p) {
        done = true
      }
    }

    if (p < center && left != null)
      result ++= left.intervalsWithPoint(p)
    if (p > center && right != null)
      result ++= right.intervalsWithPoint(p)

    result.toVector
  }

  /**
   * @param target An interval.
   * @return Every interval in this node and its children that intersects with the target interval.
   */
  def intervalsIntersectingWith(target: Interval[T]): Vector[Interval[T]] = {
    val result = new ListBuffer[Interval[T]]()
    var done = false
    val intervalEntries = intervals.entrySet().iterator()
    while (!done && intervalEntries.hasNext) {
      val entry = intervalEntries.next()

      if (entry.getKey.intersects(target)) {
        for (interval <- entry.getValue)
          result += interval
      } else if (entry.getKey.start > target.stop) {
        done = true
      }
    }

    if (target.start < center && left != null)
      result ++= left.intervalsIntersectingWith(target)
    if (target.stop > center && right != null)
      result ++= right.intervalsIntersectingWith(target)

    result.toVector
  }

  private def computeMedian(set: util.SortedSet[Long]): Long = {
    var i = 0
    val middle = set.size / 2
    val points = set.iterator()
    while (points.hasNext) {
      val point = points.next()
      if (i == middle)
        return point
      else i += 1
    }

    throw new Error("Couldn't compute median for set " + set)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= center.toString + ": "

    val intervalEntries = intervals.entrySet().iterator()
    while (intervalEntries.hasNext) {
      val entry = intervalEntries.next()
      builder ++= entry.getKey + ": {"
      builder ++= entry.getValue.mkString(", ")
      builder ++= "}"
    }

    builder.toString()
  }
}
