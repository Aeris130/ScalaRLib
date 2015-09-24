package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.RectangleCoordinates

/**
 * An area representing the dimensions and positions of a piece of a map after it has been drawn orthogonally.
 * Contains methods to modify said properties.
 */
trait MutableArea extends RectangleCoordinates {

  /**
   * @return True if the area is a room, otherwise false.
   */
  def isRoom: Boolean

  /**
   * @return True if the area is a corridor, otherwise false.
   */
  def isCorridor: Boolean

  /**
   * @return True if the area is a bend connecting two corridors, otherwise false.
   */
  def isBend: Boolean

  /**
   * @return The area covered.
   */
  def area: RectangularArea

  /**
   * @return True if the area allows its borders to intersect with other areas.
   */
  def allowsIntersection: Boolean

  /**
   * Connects another area to this one.
   * @param direction Which side of this rectangular area that the other area connects to.
   * @param connection The connection object.
   */
  def connect(direction: Direction, connection: RoomCorridorConnection): Unit

  /**
   * Tags the area as being able to move in a specified direction.
   * @param direction Direction that the area should be moved in.
   */
  def markAsMoved(direction: Direction): Unit

  /**
   * Removes the current movement tag.
   */
  def clearMovement(): Unit

  /**
   * @return The direction this room has been marked to be moved in, or None if no mark exist.
   */
  def movement: Option[Direction]

  /**
   * Checks if this area can be moved in a specified direction.
   *
   * @param direction Direction that the area should be moved in. Needs to be specified since there may be two
   *                  directions the room can move in in order to bring it closer to the target.
   * @return A list of other areas that currently block this area from moving one step in the specified direction
   *         (this includes connected corridors). An empty list if no such areas exist.
   */
  def canMove(direction: Direction): Set[MutableArea]

  /**
   * Modifies the areas coordinates by moving it one step in the direction it's currently tagged to move into.
   */
  def move(): Unit
}
