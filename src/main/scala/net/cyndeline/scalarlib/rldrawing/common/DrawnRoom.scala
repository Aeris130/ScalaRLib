package net.cyndeline.scalarlib.rldrawing.common

import net.cyndeline.rlcommon.util.Rectangle

/**
 * A single room on the game map, along with the coordinates it occupies on the final map grid.
 */
trait DrawnRoom[RoomType] {

  /**
   * @return The room object originally supplied by the user to be parsed into an actual map room.
   */
  def originalRoom: RoomType

  /**
   * @return A rectangular area represented by a start and stop coordinate (both inclusive).
   */
  def area: Rectangle

}
