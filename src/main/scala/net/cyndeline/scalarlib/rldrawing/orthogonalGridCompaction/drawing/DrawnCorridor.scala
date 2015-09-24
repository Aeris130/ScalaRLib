package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.rlcommon.util.RectangleCoordinates
import net.cyndeline.scalarlib.rldrawing.util.Connection

/**
 * A corridor, represented by a set of straight corridor segments that intersect with bends (if the final corridor
 * has any). Every corridor has 2 connections where it intersects with a room on either side of it. If the corridor has
 * bends (and thus consists of several alternating corridor/bend segments), it also has n - 1 connections that
 * specifies where the segments intersect with each other.
 *
 * Every connection is represented using two points forming a line along some axis (meaning they will share either
 * the x or y coordinate). The length of the line is the width of the connection (i.e the opening that lets the player
 * move from one area to the other).
 *
 * Note that connected areas intersect, making it so that they share the coordinates where the connection is.
 *
 * If two connected rooms (start and stop) also intersect (i.e they "share" a common wall), they will still be connected
 * using a start and stop connection that both have the same coordinates. This makes for a corridor of length 1, that is
 * only as thick as the wall shared by the rooms.
 *
 * To iterate over every segment and connection in order, begin with the start connection and the first corridor
 * segment. Then alternate between corridor segments and corridor connections until the last segment is found, and
 * finish with the stop connection.
 */
trait DrawnCorridor[RoomType, CorridorType] {

  /**
   * @return The room that this corridor starts at.
   */
  def start: RoomType

  /**
   * @return The room that this corridor stops at.
   */
  def stop: RoomType

  /**
   * @return The connection between the first rooms area and the first corridor segment.
   */
  def startConnection: Connection

  /**
   * @return The connection between the last rooms area and the last corridor segment.
   */
  def stopConnection: Connection

  /**
   * @return A list of every straight corridor segment and bend. If the corridor has no bends, this list will contain
   *         a single area entry. If bends are present, the first segment will represent a straight part of the
   *         corridor, and every other segment after that will be a rectangular bend. The segments are placed in
   *         the order they are visited when traversing the corridor from the start room to the stop room.
   */
  def corridorSegments: Vector[RectangleCoordinates]

  /**
   * @return If no bends are present, this list will be empty. Otherwise it will contain connection coordinates that
   *         mark where corridor segments meet their bends, in the order these connections are met when traversing the
   *         corridor from the start room to the stop room.
   */
  def corridorConnections: Vector[Connection]

  /**
   * @return The edge this corridor was originally based on.
   */
  def originalCorridor: CorridorType

}
