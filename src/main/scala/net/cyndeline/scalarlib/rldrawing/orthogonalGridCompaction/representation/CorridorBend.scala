package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea

/**
 * Represents a 90 degree bend in a corridor, connecting two corridor areas.
 *
 * @constructor Constructs a new corridor bend.
 * @param rectArea The area occupied by the bend.
 * @param roomGrid The grid used when checking for collisions with other areas.
 * @param intersect True if this area should be allowed to intersect the borders of other areas, otherwise false.
 */
class CorridorBend(rectArea: RectangularArea, roomGrid: PartitionedArea[MutableArea], intersect: Boolean) extends RoomArea(rectArea, roomGrid, intersect) {

  /**
   * @return True if the area is a room, otherwise false.
   */
  override def isRoom: Boolean = false

  /**
   * @return True if the area is a bend connecting two corridors, otherwise false.
   */
  override def isBend: Boolean = true

  /**
   * Connects another area to this one and enforces that connections form a bend of only 2 connections.
   * @param direction Which side of this rectangular area that the other area connects to.
   * @param c The connection object.
   */
  override def connect(direction: Direction, c: RoomCorridorConnection): Unit = {
    if (connection(direction.opposite).isDefined)
      throw new Error("Corridors connected to a corridor bend must form a 90 degree angle.")

    super.connect(direction, c)
  }

  override def toString: String = "Bend[" + area + "]"

  override def equals(other: Any) = other match {
    case cb: CorridorBend => cb.area == area
    case _ => false
  }

  override def hashCode: Int = area.##

}
