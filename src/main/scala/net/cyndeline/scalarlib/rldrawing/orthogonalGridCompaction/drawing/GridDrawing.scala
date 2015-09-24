package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom

/**
 * A final immutable drawing of a graph map where every room and corridor is represented by rectangular areas.
 */
trait GridDrawing[RoomType, CorridorType] {

  /**
   * @return Every map room and its area coordinates.
   */
  def rooms: Set[DrawnRoom[RoomType]]

  /**
   * @return Every map corridor and its coordinates, as well as where it intersects with the rooms it connects.
   */
  def corridors: Set[DrawnCorridor[RoomType, CorridorType]]

  /** @return The smallest x coordinate (inclusive). */
  def minX: Int

  /** @return The largest y coordinate (exclusive). */
  def maxX: Int

  /** @return The smallest y coordinate (inclusive). */
  def minY: Int

  /** @return The largest y coordinate (exclusive). */
  def maxY: Int

}
