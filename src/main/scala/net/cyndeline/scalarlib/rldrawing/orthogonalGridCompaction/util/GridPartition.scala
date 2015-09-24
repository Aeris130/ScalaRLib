package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.util

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.{PartitionedArea}
import net.cyndeline.scalarlib.rldrawing.util.Point
import net.cyndeline.scalarlib.rldrawing.common.RectangleCoordinates

/**
 * Partitions a 2D grid into subsections of the same size and adds objects to all partitions that the objects
 * coordinates fall into. This is done to speed up searches by limiting the number of objects to search through when
 * looking for collisions, as the only objects worth examining are the ones that are registered to the partitions
 * that belong to the area being searched through.
 *
 * @constructor Creates a grid partition with a user defined partition ratio. A higher ratio will result in fewer
 *             partitions in memory, but more objects registered to each individual partition (leading to longer
 *             searches when examining objects).
 * @param gridStart The start coordinate of the grid.
 * @param gridStop The stop coordinate of the grid. Both x and y must be higher than the start coordinate.
 * @param partitionRatio Percentage of the total grid size that each cell should represent, must be higher than
 *                       0 and less than or equal to 1.
 */
class GridPartition[E <: RectangleCoordinates : ClassTag : TypeTag] (val gridStart: Point, val gridStop: Point, partitionRatio: Double)
  extends PartitionedArea[E] {
  ratioIsValid(partitionRatio)
  areaIsPositive(gridStart, gridStop)
  private val divisorSearch = new ClosestDivisor()
  private val width = gridStop.x - gridStart.x + 1
  private val height = gridStop.y - gridStart.y + 1
  private val partitionSizeWidth = divisorSearch.findClosestDivisor(width, width / Math.ceil(width * partitionRatio).toInt)
  private val partitionSizeHeight = divisorSearch.findClosestDivisor(height, height / Math.ceil(height * partitionRatio).toInt)
  private val coordinatesPerPartitionX = width / partitionSizeWidth
  private val coordinatesPerPartitionY = height / partitionSizeHeight

  private val grid = Array.fill[Set[E]](partitionSizeWidth, partitionSizeHeight) { Set[E]() }

  /**
   * Creates a new grid starting at (0, 0) with a specified size and partition ratio.
   * @param width Width of the grid from (0, 0) to width-1.
   * @param height Height of the grid from (0, 0) to height-1.
   * @param partitionRatio Percentage of the total grid size that each cell should represent, must be higher than
   *                       0 and less than or equal to 1.
   * @return A new grid partition.
   */
  def this(width: Int, height: Int, partitionRatio: Double) = this(Point(0, 0), Point(width - 1, height - 1), partitionRatio)

  /**
   * Creates a default grid partition where each cell represents ~20% of the total coordinate set.
   * @param start The start coordinate of the grid.
   * @param stop The stop coordinate of the grid. Both x and y must be higher than the start coordinate.
   * @return A new default grid partition.
   */
  def this(start: Point, stop: Point) = this(start, stop, 0.2)

  /**
   * Finds every element registered to any partition that the rectangle represented by the start/stop coordinates
   * intersect.
   *
   * @param start Left corner of a rectangle (ex: (0, 0)).
   * @param stop Right corner of a rectangle, opposite of left (ex: (3, 6)).
   * @return A set of all elements that are members of any partition the supplied rectangle intersects.
   */
  def elementsIn(start: Point, stop: Point): Set[E] = {
    coordinatesInRange(start, stop)
    areaIsNotNegative(start, stop)

    /* When coordinates are "partitioned" into sets of the same size, the first coordinate in each partition can
     * always be divided by the partition width/height to get the index that the coordinate is represented in
     * on the grid. Likewise, as long as you round down you won't make it to the next index until you reach the
     * first coordinate in that partition.
     */
    val xStart = xAxisPartitionStart(start.x)
    val xStop = xAxisPartitionStop(stop.x)
    val yStart = yAxisPartitionStart(start.y)
    val yStop = yAxisPartitionStop(stop.y)

    (for {m <- xStart to xStop
          n <- yStart to yStop
    } yield grid(m)(n)).flatten.toSet
  }

  /**
   * Finds every element registered to a partition in a single point. This search isn't any faster than looking for a
   * range of coordinates, just syntax.
   * @param point Point of partition.
   * @return Every element member of the partition the point is in.
   */
  def elementAt(point: Point): Set[E] = elementsIn(point, point)

  /**
   * Adds an element to an area in the grid.
   * @param e Element to add.
   */
  def add(e: E): Unit = {
    coordinatesInRange(e.start, e.stop)

    val xStart = xAxisPartitionStart(e.start.x)
    val xStop = xAxisPartitionStop(e.stop.x)
    val yStart = yAxisPartitionStart(e.start.y)
    val yStop = yAxisPartitionStop(e.stop.y)

    for (m <- xStart to xStop; n <- yStart to yStop) {
      grid(m)(n) = grid(m)(n) + e
    }
  }

  /**
   * Removes an element from the grid.
   * @param e Element whose start/stop coordinates match the coordinates it's currently assigned at in the grid.
   */
  def remove(e: E): Unit = {
    coordinatesInRange(e.start, e.stop)

    val xStart = xAxisPartitionStart(e.start.x)
    val xStop = xAxisPartitionStop(e.stop.x)
    val yStart = yAxisPartitionStart(e.start.y)
    val yStop = yAxisPartitionStop(e.stop.y)

    for (m <- xStart to xStop; n <- yStart to yStop) {
      grid(m)(n) = grid(m)(n) - e
    }
  }

  /**
   * @return The number of partitioned areas along the x axis of the grid.
   */
  def partitionWidth: Int = grid.size

  /**
   * @return The number of partitioned areas along the y axis of the grid.
   */
  def partitionHeight: Int = grid(0).size

  /**
   * @return The start coordinate for the grid.
   */
  def start: Point = gridStart

  /**
   * @return The stop coordinate for the grid (inclusive).
   */
  def stop: Point = gridStop

  override def toString: String = elementsIn(gridStart, gridStop).mkString(",")

  private def xAxisPartitionStart(x: Int) = Math.floor(x / coordinatesPerPartitionX).toInt
  private def xAxisPartitionStop(x: Int) = xAxisPartitionStart(x)
  private def yAxisPartitionStart(y: Int) = Math.floor(y / coordinatesPerPartitionY).toInt
  private def yAxisPartitionStop(y: Int) = yAxisPartitionStart(y)

  private def coordinatesInRange(start: Point, stop: Point): Boolean = {
    if (start.x < gridStart.x || start.x > gridStop.x)
      throw new Error("The x start coordinate in " + start + " lies outside the grid partition area (" + gridStart + ", " + gridStop + ")")
    if (start.y < gridStart.y || start.y > gridStop.y)
      throw new Error("The y start coordinate in " + start + " lies outside the grid partition area (" + gridStart + ", " + gridStop + ")")
    if (stop.x < gridStart.x || stop.x > gridStop.x)
      throw new Error("The x stop coordinate in " + stop + " lies outside the grid partition area (" + gridStart + ", " + gridStop + ")")
    if (stop.y < gridStart.y || stop.y > gridStop.y)
      throw new Error("The y stop coordinate in " + stop + " lies outside the grid partition area (" + gridStart + ", " + gridStop + ")")

    true
  }

  /**
   * Allows 0 width or height
   */
  private def areaIsNotNegative(start: Point, stop: Point): Boolean = {
    if (start.x > stop.x || start.y > stop.y)
      throw new Error("The start x/y values must be equal or lower than the stop values (currently " + start + ", " + stop + ").")

    true
  }

  /**
   * Doesn't allow 0 width or height
   */
  private def areaIsPositive(start: Point, stop: Point): Boolean = {
    if (start.x >= stop.x || start.y >= stop.y)
      throw new Error("The start and stop values must create a non-0 size area in both width and height (currently " + start + ", " + stop + ").")

    true
  }

  private def ratioIsValid(r: Double): Boolean = {
    if (r <= 0)
      throw new IllegalArgumentException("The partition ratio must be > 0")
    else if (r > 1)
      throw new IllegalArgumentException("The partition ratio must be <= 1")

    true
  }

}
