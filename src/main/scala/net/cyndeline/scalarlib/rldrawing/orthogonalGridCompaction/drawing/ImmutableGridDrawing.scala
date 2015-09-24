package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom

/**
 * Stores area and connection data for rooms and corridors drawn using rectangular shapes on a 2D grid.
 *
 * @constructor Constructs a new immutable grid drawing.
 * @param rooms All drawn room areas.
 * @param corridors All drawn corridor areas.
 */
case class ImmutableGridDrawing[RoomType, CorridorType](rooms: Set[DrawnRoom[RoomType]],
                                                        corridors: Set[DrawnCorridor[RoomType, CorridorType]]
                                                        ) extends GridDrawing[RoomType, CorridorType] {

  /** @return The smallest x coordinate (inclusive). */
  def minX: Int = (rooms.map(_.area.start.x) ++ corridors.map(_.corridorSegments).flatten.map(_.start.x)).min

  /** @return The largest y coordinate (exclusive). */
  def maxX: Int = (rooms.map(_.area.stop.x) ++ corridors.map(_.corridorSegments).flatten.map(_.stop.x)).max

  /** @return The smallest y coordinate (inclusive). */
  def minY: Int = (rooms.map(_.area.start.y) ++ corridors.map(_.corridorSegments).flatten.map(_.start.y)).min

  /** @return The largest y coordinate (exclusive). */
  def maxY: Int = (rooms.map(_.area.stop.y) ++ corridors.map(_.corridorSegments).flatten.map(_.stop.y)).max

}
