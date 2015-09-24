package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.help.ConnectionBoundary

/**
 * Connects a room (or a corridor bend) to a single corridor.
 *
 * @constructor Constructs a new connection.
 * @param room Room connected to the corridor.
 * @param corridor Corridor connected to the room.
 * @param roomConnectDirection The side of the room that the corridor connects to.
 * @param deviation The number of tiles the corridor is allowed to deviate from the middle.
 */
class Connection(val room: RoomArea, val corridor: CorridorArea, val roomConnectDirection: Direction, val deviation: Int)
  extends RoomCorridorConnection {

  if (deviation < 0) throw new Error("Corridor boundary deviation must be >= 0, currently " + deviation + ".")

  if (corridor.area.lengthOfSide(roomConnectDirection.opposite) > room.area.lengthOfSide(roomConnectDirection))
    throw new IllegalArgumentException("The " + roomConnectDirection.opposite + " side of the corridor " +
      corridor + " is larger than the " + roomConnectDirection + " of the room " + room + " it connects to.")

  checkDisconnect // Needs to come before out of bounds, since bounds is a subset of this exception
  checkOutOfBounds

  /**
   * Constructs a connection with a deviation equal to the longest side of the room area, allowing the corridor to
   * move across the entire boundary.
   * @param room Room connected to the corridor.
   * @param corridor Corridor connected to the room.
   * @param roomConnectDirection The side of the room that the corridor connects to.
   * @return A corridor connection with maximum deviation allowed.
   */
  def this(room: RoomArea, corridor: CorridorArea, roomConnectDirection: Direction) =
    this(room, corridor, roomConnectDirection, Math.max(room.area.lengthOfSide(West), room.area.lengthOfSide(North)))

  /**
   * Checks if the room can move 1 step without moving the corridor, i.e if the corridor can "slide" across the room
   * boundary without leaving it. This check only compares boundaries between corridor and room, actual movement
   * coordinates and colliding objects are not taken into account.
   * @param direction Direction to mode room into.
   * @return True if the room can move without moving the corridor, otherwise false.
   */
  def roomCanMoveIndependently(direction: Direction): Boolean = {

    if (direction == roomConnectDirection || direction == roomConnectDirection.opposite) {
      false
    } else {

      /* Checking if the room can be moved along the axis the corridor connects to in one direction is the same as
       * checking if the corridor can move in the other.
       */
      corridorCanMoveIndependently(direction.opposite)
    }

  }

  /**
   * Checks if the corridor can move 1 step without moving the room, i.e if the corridor can "slide" across the room
   * boundary without leaving it. This check only compares boundaries between corridor and room, actual movement
   * coordinates and colliding objects are not taken into account.
   * @param direction Direction to move corridor into.
   * @return True if the corridor can move without moving the room, otherwise false.
   */
  def corridorCanMoveIndependently(direction: Direction): Boolean = {
    val boundaryComputation = new ConnectionBoundary(room.area, deviation)
    val modification = direction match {
      case North => -1
      case West => -1
      case South => 1
      case East => 1
    }

    if (direction != roomConnectDirection && direction != roomConnectDirection.opposite) {
      val boundary: (Int, Int) = boundaryComputation.computeConnectionBoundary(corridor.area, roomConnectDirection)
      val corridorCoordinates: (Int, Int) = corridor.area.coordinatesOnSide(direction.parallel)
      val corridorCoordinatesAfterMovement = (corridorCoordinates._1 + modification, corridorCoordinates._2 + modification)

      corridorCoordinatesAfterMovement._1 >= boundary._1 && corridorCoordinatesAfterMovement._2 <= boundary._2

    } else {
      false
    }
  }

  private def checkOutOfBounds: Boolean = {
    val roomC = room.area.coordinatesOnSide(roomConnectDirection)
    val roomStart = roomC._1
    val roomStop = roomC._2

    val corridorC = corridor.area.coordinatesOnSide(roomConnectDirection.opposite)
    val corridorStart = corridorC._1
    val corridorStop = corridorC._2

    if (roomStart > corridorStart || roomStop < corridorStop)
      throw new IllegalArgumentException("The corridor " + corridor + " must be within bounds of the room " +
        room + "'s start and stop coordinates in order to connect to it.")

    true
  }

  private def checkDisconnect: Boolean = {
    val roomC = room.area.coordinatesOnSide(roomConnectDirection.parallel)
    val roomStart = roomC._1
    val roomStop = roomC._2

    val corridorC = corridor.area.coordinatesOnSide(roomConnectDirection.parallel)
    val corridorStart = corridorC._1
    val corridorStop = corridorC._2

    if (roomStart != corridorStart && roomStart != corridorStop && roomStop != corridorStart && roomStop != corridorStop)
      throw new Error("The room " + room + " and corridor " + corridor + " does not intersect at their borders.")

    true
  }

}
