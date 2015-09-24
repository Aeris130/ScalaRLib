package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction

import net.cyndeline.rlcommon.util.WidthConstraint

/**
 * Defines properties regarding position and size for corridor area segments.
 */
trait CorridorProperties extends WidthConstraint {

  /**
   * @return A lower bound on the length of this corridor when compacting the map. If the corridor gets drawn with
   *         a lower initial length than this value, it will not be increased to match it.
   *         Must be equal or higher than 1.
   */
  def minimumLength: Int

  /**
   * @return The number of tiles a corridor may slide along the side of the room it is connected to when compacting
   *         the map. A value of 0 means that the corridor will always remain in the middle of the room boundary.
   *         The user does not need to specify deviations that guarantee that the corridor does not leave the room
   *         boundary, in such case the start/stop of the boundary will be used instead (this is needed since the user
   *         can't tell which side of a room a corridor will be connected to). Must be equal or higher than 0.
   */
  def boundaryDeviation: Int
}
