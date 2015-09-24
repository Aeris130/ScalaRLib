package net.cyndeline.scalarlib.rldungeon.grammar.util

import net.cyndeline.scalarlib.rldungeon.common.Room

/**
 * Modifies a new room that's about to be inserted by a topology production.
 */
trait RoomModification[T <: Room] {

  /**
   * @param room A newly produced room.
   * @return A copy of the room to insert.
   */
  def modify(room: T): T
}
