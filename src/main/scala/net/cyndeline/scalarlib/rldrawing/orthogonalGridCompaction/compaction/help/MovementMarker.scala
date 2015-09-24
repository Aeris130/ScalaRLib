package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.help

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.MutableArea
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * Sets movement data in a set of areas, or returns false if one of them cannot move in the specified direction.
 * @param target The compaction target point that all areas attempt to move towards. Areas that has reached this
 *               point on any axis will not be moved further in that direction.
 * @param min The lowest x/y coordinates a room may move to.
 * @param max The highest x/y coordinates a room may move to.
 */
class MovementMarker(target: Point, min: Point, max: Point) {

  /**
   * @param areas Areas to set movement for.
   * @param direction Movement direction to set.
   * @return True if every area had its movement set, false if the iteration was interrupted due to an area
   *         having reached the target on the same axis as it is being moved on. Some areas may still have
   *         movement set.
   */
  def markAreasForMovement(areas: Set[MutableArea], direction: Direction): Boolean = {
    val allAreas = areas.iterator
    while (allAreas.hasNext) {
      val a = allAreas.next()

      // While there's no limit to how high the area coordinates may go, coordinates cannot be negative.
      direction match {
        case North => if (a.start.y <= min.x) return false
        case West => if (a.start.x <= min.y) return false
        case South => if (a.stop.y >= max.y) return false
        case East => if (a.stop.x >= max.x) return false
      }

      /* Don't move the area if the target lies within it on the movement axis
       *
       * Since only rooms and bends can be selected to have the target inside them, if a corridor has reached a
       * target it's because the target ended up on the edge of a room/bend, and the corridor is now adjacent to it.
       * The corridor should still be able to move when shrinking itself though, so it is moved anyway.
       */
      if (!a.isCorridor) {
        direction match {
          case North | South => if (a.start.y <= target.y && a.stop.y >= target.y) return false
          case West | East => if (a.start.x <= target.x && a.stop.x >= target.x) return false
        }
      }

      a.markAsMoved(direction)
    }

    true
  }
}
