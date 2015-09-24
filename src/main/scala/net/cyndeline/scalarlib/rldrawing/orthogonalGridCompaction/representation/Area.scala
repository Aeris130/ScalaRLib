package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation

import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * Base methods for moving and connecting areas.
 *
 * @param rectArea Rectangular coordinates for this area.
 * @param intersectingBorders True if this area shouldn't consider other areas as obstructing its movement until they
 *                            intersect its borders. If false, other areas will be considered obstructing as soon as
 *                            they're adjacent to this area.
 */
abstract class Area(rectArea: RectangularArea, intersectingBorders: Boolean) extends MutableArea {

  /* Corridor connections. one for each side of the room. */
  var northCorridor: Option[RoomCorridorConnection] = None
  var southCorridor: Option[RoomCorridorConnection] = None
  var westCorridor: Option[RoomCorridorConnection] = None
  var eastCorridor: Option[RoomCorridorConnection] = None

  /* Modify this when moving the area */
  private var mutableArea = rectArea

  private var currentMovement: Option[Direction] = None

  /**
   * @return The starting point for this rectangular area, must be equal or less than the stop value.
   */
  def start: Point = mutableArea.start

  /**
   * @return The ending coordinate (inclusive) for this rectangular area.
   */
  def stop: Point = mutableArea.stop

  /**
   * @return The area object representing position and dimensions of this room.
   */
  def area: RectangularArea = mutableArea

  /**
   * @return True if the area allows its borders to intersect with other areas.
   */
  def allowsIntersection: Boolean = intersectingBorders

  /**
   * Connects another area to this one.
   * @param direction Which side of this rectangular area that the other area connects to.
   * @param connection The connection object.
   */
  def connect(direction: Direction, connection: RoomCorridorConnection): Unit = direction match {
    case North => northCorridor = Option(connection)
    case South => southCorridor = Option(connection)
    case West => westCorridor = Option(connection)
    case East => eastCorridor = Option(connection)
  }

  /**
   * Tags the area as being able to move in a specified direction.
   * @param direction Direction that the area should be moved in.
   */
  def markAsMoved(direction: Direction): Unit = {
    if (currentMovement.isEmpty) currentMovement = Option(direction)
    else throw new Error("Cannot mark a room for movement twice without resetting.")
  }

  /**
   * Removes the current movement tag.
   */
  def clearMovement(): Unit = {
    if (currentMovement.isDefined) currentMovement = None
    else throw new Error("Cannot reset a rooms movement when no movement has been added.") // Discourages user to iterate over the entire area set
  }

  /**
   * @return The direction this room has been marked to be moved in, or None if no mark exist.
   */
  def movement: Option[Direction] = currentMovement

  /**
   * Retrieves the connection of a specified direction.
   * @param direction Direction of connection to retrieve.
   * @return The retrieved connection, or None if no connection exist for that direction.
   */
  def connection(direction: Direction): Option[RoomCorridorConnection] = direction match {
    case North => northCorridor
    case South => southCorridor
    case West => westCorridor
    case East => eastCorridor
  }

  /**
   * Sets a new rectangle representing this area.
   * @param area New area to use.
   */
  def setNewArea(area: RectangularArea) {
    mutableArea = area
  }

  /**
   * Finds areas adjacent (shares coordinates with one of the borders) to this area when moving in a
   * specified direction. If an area only intersects a corner, it won't be obstructing when this area moves,
   * and is thus not considered adjacent. Areas present in connections are not returned.
   *
   * @param direction Direction to move area in.
   * @param grid Grid containing all areas, including this one.
   * @return Every adjacent area.
   */
  final def adjacentNonConnectedAreas(direction: Direction, grid: PartitionedArea[MutableArea]): Set[MutableArea] = {
    val areaPoints = area.coordinatesForSide(direction)

    /* If we're looking for intersecting areas, the current points will do fine. When looking for adjacent areas,
     * the coordinates need to be adjusted outward by 1.
     */
    val areaFromPoints = if (intersectingBorders) {
      new RectangularArea(areaPoints._1, areaPoints._2)
    } else {

      /* Make sure the new coordinates doesn't go outside the grid. Since the purpose of this call is now to
       * find adjacent neighbors rather than intersecting ones, if the coordinates of this area lies on the grid
       * boundary, no areas can exist in the moving direction.
       *
       * Technically, this should never happen since a room shouldn't be asked to move outside the grid.
       */
      val gridArea = new RectangularArea(grid.start, grid.stop)

      direction match {
        // Doesn't matter which point is used since they're both on the same axis
        case North => if (areaPoints._1.y <= gridArea.start.y ) return Set()
        case South => if (areaPoints._1.y >= gridArea.stop.y) return Set()
        case West => if (areaPoints._1.x <= gridArea.start.x) return Set()
        case East => if (areaPoints._1.x >= gridArea.stop.x) return Set()
      }
      new RectangularArea(areaPoints._1, areaPoints._2).adjustCoordinates(direction, 1)
    }

    /* Either contains all areas that might possibly intersect the border, or the ones that lie
     * 1 coordinate to the side of it.
     */
    val areasToExamine = grid.elementsIn(areaFromPoints.start, areaFromPoints.stop)

    val blockingAreas = for {
      mutableArea <- areasToExamine
      oppositeSideCoordinates = mutableArea.area.coordinatesForSide(direction.opposite)
      coordinateArea = RectangularArea(oppositeSideCoordinates._1, oppositeSideCoordinates._2)

      if !mutableArea.movement.isDefined // No need to report areas that has already been confirmed to move
      if !areaIsConnection(mutableArea, direction)

      if (intersectingBorders
        && area.overlaps(coordinateArea)
        // Areas that only intersect in a corner won't be in the way when moving, if intersection is allowed
        && area.start != coordinateArea.stop
        && area.stop != coordinateArea.start) || (!intersectingBorders && area.adjacentTo(coordinateArea, direction))

    } yield mutableArea

    blockingAreas
  }

  private def areaIsConnection(area: MutableArea, direction: Direction): Boolean = {
    val c = connection(direction)
    c.isDefined && (c.get.corridor == area || c.get.room == area)
  }

}
