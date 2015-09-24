package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{Point, RectangleCoordinates}
import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.AreaRepresentation
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{FullCorridor, MutableArea, RectangularArea}
import net.cyndeline.scalarlib.rldrawing.util.Connection

/**
 * Used for injection.
 */
trait RepresentationToDrawingConverterI {
  def convertToDrawing[RoomType, CorridorType](representation: AreaRepresentation[RoomType, CorridorType]): GridDrawing[RoomType, CorridorType]
}

/**
 * Parses a mutable orthogonal area representation into an immutable drawing.
 *
 * @constructor Constructs a new converter.
 */
class RepresentationToDrawingConverter extends RepresentationToDrawingConverterI {

  def convertToDrawing[RoomType, CorridorType](representation: AreaRepresentation[RoomType, CorridorType]): GridDrawing[RoomType, CorridorType] = {

    /* Used to calculate how much coordinates should be shifted on the x/y axis to end up as close as
     * possible to (0, 0).
     */
    val lowestXAndY: (Int, Int) = findLowestXAndY(representation)
    val rooms: Set[DrawnRoom[RoomType]] = (for (r <- representation.roomMap) yield {
      ImmutableRoomContainer(r._1, adjustArea(r._2.area, lowestXAndY))
    }).toSet
    val areaToOriginal = representation.roomMap.map(_.swap)
    val corridors = (for {originalAndFullCorridor <- representation.corridorMap
                        } yield makeCorridor(originalAndFullCorridor._1, areaToOriginal, originalAndFullCorridor._2, lowestXAndY)).toSet

    ImmutableGridDrawing(rooms, corridors)
  }

  /**
   * Parses area data and original rooms into a drawn corridor.
   * @param originalCorridor The original user-submitted corridor object these areas are based on.
   * @param areaToRoom Since the FullCorridor objects doesn't contain references to the original rooms, this is needed.
   * @param corridor The areas of the corridor.
   */
  private def makeCorridor[RoomType, CorridorType]
                          (originalCorridor: CorridorType,
                           areaToRoom: Map[MutableArea, RoomType],
                           corridor: FullCorridor,
                           lowestXAndY: (Int, Int)): DrawnCorridor[RoomType, CorridorType] = {

    /* Since all areas of a corridor only intersect at their connections, the intersection rectangle of each area pair
     * can be used as connection data.
     */
    val from = adjustArea(corridor.from.area, lowestXAndY)
    var currentArea = from

    var fromConnection: Option[(Point, Point)] = None
    var connections = Vector[(Point, Point)]()
    var segmentAreas = Vector[RectangleCoordinates]()

    // Does not contain room areas
    val corridorSegments = corridor.areas.iterator
    while (corridorSegments.hasNext) {
      val segment = corridorSegments.next()
      val adjustedArea = adjustArea(segment.area, lowestXAndY)

      val intersection = getIntersection(currentArea, adjustedArea)

      segmentAreas = adjustedArea +: segmentAreas

      if (fromConnection.isEmpty) {
        fromConnection = Option((intersection.start, intersection.stop))
      } else {
        connections = (intersection.start, intersection.stop) +: connections
      }

      currentArea = adjustedArea
    }

    val to = adjustArea(corridor.to.area, lowestXAndY)
    val toConnection = getIntersection(currentArea, to)
    val toConnectionPoints = (toConnection.start, toConnection.stop)

    // Reverse the lists since the above loop used prepends to add elements in constant time.
    ImmutableCorridorContainer(areaToRoom(corridor.from), areaToRoom(corridor.to), Connection(fromConnection.get), Connection(toConnectionPoints), segmentAreas.reverse, connections.reverse.map(Connection(_)), originalCorridor)
  }

  private def getIntersection(a: RectangularArea, b: RectangularArea): RectangularArea = {
    val inter = a.intersection(b).getOrElse {
      throw new Error("The area " + a + " did not intersect with the corridor segment " + b)
    }

    if (inter.lengthOfSide(North) != 1 && inter.lengthOfSide(West) != 1)
      throw new Error("The area " + a + " intersected with the corridor segment " + b + " inside the border, rather than at the border.")

    inter
  }

  private def findLowestXAndY[RoomType, CorridorType](representation: AreaRepresentation[RoomType, CorridorType]): (Int, Int) = {
    var lowestX = Int.MaxValue
    var lowestY = Int.MaxValue
    for (room <- representation.rooms) {
      if (room.start.x < lowestX) lowestX = room.start.x
      if (room.start.y < lowestY) lowestY = room.start.y
    }

    /* No need to check every corridor segment area since bends will always be at the beginning
     * and end of every segment.
     */
    for (bend <- representation.bends) {
      if (bend.start.x < lowestX) lowestX = bend.start.x
      if (bend.start.y < lowestY) lowestY = bend.start.y
    }

    (lowestX, lowestY)
  }

  private def adjustArea(area: RectangularArea, adjustOnXAndY: (Int, Int)): RectangularArea = {
    area.adjustCoordinates(West, adjustOnXAndY._1).adjustCoordinates(North, adjustOnXAndY._2)
  }

}
