package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea

/**
 * A rectangular boundary of a single room on the map, including a set of wall-tiles surrounding it. Because of this,
 * a room with a single tile must be 3x3 tiles in size (hence why 3 is the minimum room size).
 *
 * @constructor Constructs a new room area.
 * @param rectArea Rectangular coordinates for this area.
 * @param grid Grid containing all areas, including this one.
 * @param intersect True if this area should be allowed to intersect the borders of other areas, otherwise false.
 */
class RoomArea(rectArea: AdjustableRectangle, grid: PartitionedArea[MutableArea], intersect: Boolean) extends Area(rectArea, intersect) {
  if (area.lengthOfSide(North) < 3) throw new IllegalArgumentException("Rooms must be of minimum size 3x3, currently the x axis has length " + area.lengthOfSide(North) + " in the area " + area)
  if (area.lengthOfSide(West) < 3) throw new IllegalArgumentException("Rooms must be of minimum size 3x3, currently the y axis has length " + area.lengthOfSide(West) + " in the area " + area)

  /**
   * @return True if the area is a room, otherwise false.
   */
  override def isRoom: Boolean = true

  /**
   * @return True if the area is a corridor, otherwise false.
   */
  override def isCorridor: Boolean = false

  /**
   * @return True if the area is a bend connecting two corridors, otherwise false.
   */
  override def isBend: Boolean = false

  /**
   * Checks if this area can be moved in a specified direction.
   *
   * @param direction Direction that the area should be moved in. Needs to be specified since there may be two
   *                  directions the room can move in in order to bring it closer to the target.
   * @return A list of other areas that currently block this area from moving one step in the specified direction
   *         (this includes connected corridors). An empty list if no such areas exist.
   */
  override def canMove(direction: Direction): Set[MutableArea] = {
    connectionsToMove(direction, northCorridor, southCorridor, westCorridor, eastCorridor) ++
      adjacentNonConnectedAreas(direction, grid) - this // This room is adjacent to itself
  }

  /**
   * Modifies the areas coordinates by moving it one step in the direction it's currently tagged to move into.
   */
  override def move(): Unit = {
    if (!movement.isDefined)
      throw new Error("Attempted to move room " + this + " but no movement direction was set.")

    grid.remove(this)
    setNewArea(area.adjustCoordinates(movement.get, 1))
    grid.add(this)
  }

  override def toString: String = "RoomArea[" + area + "]"

  override def equals(other: Any) = other match {
    case r: RoomArea => r.area == area
    case _ => false
  }

  override def hashCode: Int = rectArea.##

  /**
   * Appends every connection that must be moved along with this room to a list.
   */
  private def connectionsToMove(direction: Direction, c: Option[RoomCorridorConnection]*): Set[MutableArea] = {
    (for {
      connection <- c
      if connection.isDefined
      if connection.get.corridor.movement.isEmpty
      if !connection.get.roomCanMoveIndependently(direction)
    } yield connection.get.corridor).toSet
  }

}
