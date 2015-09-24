package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.scalarlib.rldrawing.util.Direction._

/**
 * Connects a room and a corridor.
 */
trait RoomCorridorConnection {

  /**
   * @return The connected room.
   */
  def room: MutableArea

  /**
   * @return The connected corridor.
   */
  def corridor: MutableArea

  /**
   * Checks if the room can move 1 step without moving the corridor, i.e if the corridor can "slide" across the room
   * boundary without leaving it. This check only compares boundaries between corridor and room, actual movement
   * coordinates and colliding objects are not taken into account.
   * @param direction Direction to mode room into.
   * @return True if the room can move without moving the corridor, otherwise false.
   */
  def roomCanMoveIndependently(direction: Direction): Boolean

  /**
   * Checks if the corridor can move 1 step without moving the room, i.e if the corridor can "slide" across the room
   * boundary without leaving it. This check only compares boundaries between corridor and room, actual movement
   * coordinates and colliding objects are not taken into account.
   * @param direction Direction to mode corridor into.
   * @return True if the corridor can move without moving the room, otherwise false.
   */
  def corridorCanMoveIndependently(direction: Direction): Boolean

}
