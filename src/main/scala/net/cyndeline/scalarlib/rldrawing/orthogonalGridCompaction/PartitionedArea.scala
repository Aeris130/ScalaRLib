package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction

import net.cyndeline.rlcommon.math.geom.Point

/**
 * Partitions a 2D grid into subsections of the same size and adds objects to all partitions that the objects
 * coordinates fall into. This is done to speed up searches by limiting the number of objects to search through when
 * looking for collisions, as the only objects worth examining are the ones that are registered to the partitions
 * that belong to the area being searched through.
 *
 * @tparam E Object type stored in the grid.
 */
trait PartitionedArea[E] {

  /**
   * Finds every element registered to any partition that the rectangle represented by the start/stop coordinates
   * intersect.
   *
   * @param start Left corner of a rectangle (ex: (0, 0)).
   * @param stop Right corner of a rectangle, opposite of left (ex: (3, 6)).
   * @return A set of all elements that are members of any partition the supplied rectangle intersects.
   */
  def elementsIn(start: Point, stop: Point): Set[E]

  /**
   * Finds every element registered to a partition in a single point. This search isn't any faster than looking for a
   * range of coordinates, just syntax.
   * @param point Point of partition.
   * @return Every element member of the partition the point is in.
   */
  def elementAt(point: Point): Set[E]

  /**
   * Adds an element to an area in the grid.
   * @param e Element to add.
   */
  def add(e: E): Unit

  /**
   * Removes an element from the grid.
   * @param e Element whose start/stop coordinates match the coordinates it's currently assigned at in the grid.
   */
  def remove(e: E): Unit

  /**
   * @return The number of partitioned areas along the x axis of the grid.
   */
  def partitionWidth: Int

  /**
   * @return The number of partitioned areas along the y axis of the grid.
   */
  def partitionHeight: Int

  /**
   * @return The start coordinate for the grid.
   */
  def start: Point

  /**
   * @return The stop coordinate for the grid (inclusive).
   */
  def stop: Point

}
