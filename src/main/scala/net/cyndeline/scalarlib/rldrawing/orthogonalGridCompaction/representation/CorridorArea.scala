package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.util.Direction._

/**
 * Represents a straight rectangular corridor section. Corridors with bends are represented by multiple CorridorAreas,
 * joined using corridor bends.
 *
 * @constructor Constructs a new corridor area.
 * @param rectArea Rectangular coordinates for this area.
 * @param grid Grid containing all areas, including this one.
 * @param connectionDir Specifies which side of this corridor (and its opposite side) that can hold connections to
 *                      rooms.
 * @param minLength The minimum length that the corridor must have in the direction its connections are (ex: a corridor
 *                  connecting on its west and east sides computes its length by measuring the north/south side).
 *                  The corridor will never shrink itself below this length when moving.
 * @param intersect True if this area should be allowed to intersect the borders of other areas, otherwise false.
 */
class CorridorArea(rectArea: RectangularArea,
                   grid: PartitionedArea[MutableArea],
                   val connectionDir: Direction,
                   val minLength: Int,
                   intersect: Boolean) extends Area(rectArea, intersect) {
  if (rectArea.lengthOfSide(connectionDir) < 3)
    throw new Error("Corridors must be of minimum size 3 in the direction they connect, currently " +
      this + " has size " + rectArea.lengthOfSide(connectionDir) + "in the " + connectionDir + " direction")

  /**
   * Constructs a corridor with minimum length 1.
   * @param rectArea Rectangular coordinates for this area.
   * @param grid Grid containing all areas, including this one.
   * @param connectionDir Specifies which side of this corridor (and its opposite side) that can hold connections to
   *                      rooms.
   * @param intersect True if this area should be allowed to intersect the borders of other areas, otherwise false.
   * @return A corridor without minimum length.
   */
  def this(rectArea: RectangularArea, grid: PartitionedArea[MutableArea], connectionDir: Direction, intersect: Boolean) =
    this(rectArea, grid, connectionDir, 1, intersect)

  /**
   * @return True if the area is a room, otherwise false.
   */
  def isRoom: Boolean = false

  /**
   * @return True if the area is a corridor, otherwise false.
   */
  def isCorridor: Boolean = true

  /**
   * @return True if the area is a bend connecting two corridors, otherwise false.
   */
  def isBend: Boolean = false

  /**
   * Connects another area to this one. Corridors only allows two connections, and only in opposite
   * directions.
   * @param direction Which side of this rectangular area that the other area connects to.
   * @param connection The connection object.
   */
  override def connect(direction: Direction, connection: RoomCorridorConnection): Unit = {
    val parallel = direction.parallel

    if (this.connection(parallel).isDefined || this.connection(parallel.opposite).isDefined)
      throw new Error("Corridor connections must face each other. " + this + " cannot connect a corridor to the " + direction + " side.")

    direction match {
      case North => northCorridor = Option(connection)
      case South => southCorridor = Option(connection)
      case West => westCorridor = Option(connection)
      case East => eastCorridor = Option(connection)
    }
  }

  /**
   * Checks if this area can be moved in a specified direction.
   *
   * @param direction Direction that the area should be moved in. Needs to be specified since there may be two
   *                  directions the room can move in in order to bring it closer to the target.
   * @return A list of other areas that currently block this area from moving one step in the specified direction
   *         (this includes connected corridors). An empty list if no such areas exist.
   */
  override def canMove(direction: Direction): Set[MutableArea] = {
    var areas = adjacentNonConnectedAreas(direction, grid) ++
      connectionsToMove(direction, northCorridor, southCorridor, westCorridor, eastCorridor) - this // This corridor is adjacent to itself

    /* If the connection the corridor is moving into isn't set to move, no need to move it. The corridor can just
     * shrink instead as long as it hasn't been shrunk to the minimum length.
     */
    val movementDirConnection = connection(direction)
    if (movementDirConnection.isDefined && movementDirConnection.get.room.movement.isEmpty) {

      if (area.lengthOfSide(direction.parallel) > minLength)
        areas -= movementDirConnection.get.room
    }

    areas
  }

  /**
   * Modifies the areas coordinates by moving it one step in the direction it's currently tagged to move into.
   */
  override def move(): Unit = {
    val moveDir = movement.getOrElse {
      throw new Error("Attempted to move corridor " + this + " but no movement direction was set.")
    }

    val connection1 = connection(moveDir)
    val connection2 = connection(moveDir.opposite)

    if (connection1.isEmpty && connection2.isEmpty) { // Moving parallel to this corridors rooms
      setNewArea(area.adjustCoordinates(moveDir, 1))

    } else if (connection1.isDefined && connection2.isDefined) { // Moving towards a room
      val c1Room = connection1.get.room
      val c2Room = connection2.get.room
      movementDirectionCheck(c1Room, c2Room)

      /* Connection 1 lies in the movement direction, if its room is set to move, move this corridor as well.
       * Otherwise shrink it unless minimum length is already reached.
       */
      if (c1Room.movement.isDefined) {
        setNewArea(area.adjustCoordinates(moveDir, 1))

      } else if (area.lengthOfSide(moveDir.parallel) > minLength) { // Shrink
        setNewArea(area.reduceFromSide(moveDir, 1))
        setNewArea(area.adjustCoordinates(moveDir, 1))

      } else {
        throw new Error("The corridor " + this + " was set to move " + moveDir + ", but it has reached its minimum length and the room connected in that direction is not set to move.")
      }

    } else {
      throw new Error("Attempted to move corridor " + this + ", but both end connections have not been set.")
    }
  }

  override def toString: String = "CorridorArea[" + area + " -> " + connectionDir + "]"

  override def equals(other: Any) = other match {
    case ca: CorridorArea => ca.area == area && ca.connectionDir == connectionDir && ca.minLength == minLength && ca.allowsIntersection == allowsIntersection
    case _ => false
  }

  override def hashCode: Int = area.## ^ connectionDir.## ^ minLength.## ^ allowsIntersection.##

  private def movementDirectionCheck(r1: MutableArea, r2: MutableArea) {
    if (r1.movement.isDefined && r2.movement.isDefined && r1.movement.get != r2.movement.get)
      throw new Error("Attempted to move rooms " + r1 + " and " + r2 + " in different directions.")
  }

  private def connectionsToMove(direction: Direction, c: Option[RoomCorridorConnection]*): Set[MutableArea] = {
    (for {
      connection <- c
      if connection.isDefined
      if !connection.get.room.movement.isDefined
      if !connection.get.corridorCanMoveIndependently(direction)
    } yield connection.get.room).toSet
  }
}
