package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.Geom
import net.cyndeline.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.{FloorPlan, RoomArea}

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Reduces the size of areas that exceed their target size by moving individual sides of the area inwards. This
 * occurs until the target size is reached, or no additional reduction can occur without violating size or aspect
 * ratio constraints. As gates are inherently less interesting than rooms, they are reduced first as to prevent room
 * reductions from limiting their scaling.
 *
 * Note that unlike optimizations performed on rectangular duals, this class adjusts the individual sides of each
 * rectangular area rather than the maximal segments.
 *
 * @param roomAspectRatio The highest aspect ratio allowed when reducing the size of a room.
 * @param gateAspectRatio The highest aspect ratio allowed when reducing the size of a gate.
 * @param minimumRoomSide The minimum amount of coordinate along any axis that every room in the plan should have.
 * @param minimumGateSide The minimum amount of coordinate along any axis that every gate in the plan should have.
 */
class SegmentReducer(roomAspectRatio: Double, gateAspectRatio: Double, minimumRoomSide: Int, minimumGateSide: Int) {

  /**
   * Reduces individual areas in a floor plan.
   * @param plan A floor plan.
   * @return A new floor plan with its room areas reduced in size if possible without breaking their connections
   *         to neighbors, and without going beneath the target size.
   */
  def reduceFloorPlan[V <: MapArea, E[X] <: UnDiEdge[X]](plan: FloorPlan[V, E]): FloorPlan[V, E] = {
    reductionAlgorithm(plan, roomAspectRatio, gateAspectRatio, minimumRoomSide, minimumGateSide, None)
  }

  /**
   * Reduces individual areas in a floor plan and ensures that the entry room has the outermost coordinate in every
   * direction that it faces outside the plan. This method should only be used if the room is present on the outside of
   * the floor plan, and is used to enter the layout from outside.
   *
   * @param plan A floor plan.
   * @param entry The room used to enter the floor plan from outside.
   * @return A new floor plan with its room areas reduced in size if possible without breaking their connections
   *         to neighbors, and without going beneath the target size.
   */
  def reduceFloorPlanWithEntry[V <: MapArea, E[X] <: UnDiEdge[X]](plan: FloorPlan[V, E], entry: V): FloorPlan[V, E] = {
    reductionAlgorithm(plan, roomAspectRatio, gateAspectRatio, minimumRoomSide, minimumGateSide, Some(entry))
  }

  def reductionAlgorithm[V <: MapArea, E[X] <: UnDiEdge[X]](plan: FloorPlan[V, E],
                                                            roomAspectRatio: Double,
                                                            gateAspectRatio: Double,
                                                            minimumRoomSide: Int,
                                                            minimumGateSide: Int,
                                                            entry: Option[V]): FloorPlan[V, E] = {

    /* One scaler for rooms, and one for gates since they may have different requirements. */
    val roomScaler = new RectangleScaler(roomAspectRatio, minimumRoomSide)
    val gateScaler = new RectangleScaler(gateAspectRatio, minimumGateSide)

    var currentFloorPlan = plan

    for (gate <- plan.roomAreas.filter(_.isGate)) {

      /* Target size 9 (3x3) is used since all gates should be made as small as possible. */
      val scaledCoordinates = gateScaler.scaleDown(gate, 9, currentFloorPlan.intersections(gate))
      if (scaledCoordinates.isDefined)
        currentFloorPlan = currentFloorPlan.updateRooms(Vector(gate -> scaledCoordinates.get))
    }

    /* Every non-entry room is scaled first, before checking how far the entry can scale. */
    for (room <- plan.roomAreas.filter(r => r.isRoom && (entry.isEmpty || (entry.isDefined && r.originalRoom != entry.get)))) {
      val scaledCoordinates = roomScaler.scaleDown(room, room.originalRoom.targetArea, currentFloorPlan.intersections(room))
      if (scaledCoordinates.isDefined)
        currentFloorPlan = currentFloorPlan.updateRooms(Vector(room -> scaledCoordinates.get))
    }

    /* Finally, the entry room is handled as a special case, as it requires the remaining rooms to scale down before
     * it becomes possible to see what the outermost non-entry room is on each side that the entry-room is facing.
     * The entry will never be scaled below this coordinate, as it must always be on the outer border of the floor-
     * plan in order for the inside to make sense with a rectangular outside when representing the floors as a
     * building.
     */
    if (entry.isDefined) {
      currentFloorPlan = shrinkEntryArea(currentFloorPlan, roomScaler, entry.get)
    }

    currentFloorPlan
  }

  private def shrinkEntryArea[V <: MapArea, E[X] <: UnDiEdge[X]](plan: FloorPlan[V, E],
                                                                 scaler: RectangleScaler,
                                                                 entry: V): FloorPlan[V, E] = {
    var currentFloorPlan = plan
    val entryRoom = currentFloorPlan.roomAreas.find(r => r.isRoom && r.originalRoom == entry)
      .getOrElse(throw new Error("Could not find an area in the floor plan corresponding to the submitted entry room " + entry + "."))
    val entryFacingSides = entrySides(plan, entryRoom)
    require(!entryFacingSides.isEmpty, "The entry room could not find any outer sides it were facing in.")

    // The outermost coordinate in every direction that the entry is facing.
    val outerCoordinates = for {
      direction <- entryFacingSides
      coordinatesInDirection = currentFloorPlan.roomAreas
        .filter(r => r.isGate || (r.isRoom && r.originalRoom != entry))
        .map(area => Geom.furthestCoordinate(direction, area))

      outermostCoordinate = direction match {
        case North | West => coordinatesInDirection.min
        case South | East => coordinatesInDirection.max
      }
    } yield (direction, outermostCoordinate)

    val newEntryRoomCoordinates = scaler.scaleDownEntry(entryRoom, entryRoom.originalRoom.targetArea, currentFloorPlan.intersections(entryRoom), outerCoordinates)
    if (newEntryRoomCoordinates.isDefined) {
      currentFloorPlan = currentFloorPlan.updateRooms(Vector(entryRoom -> newEntryRoomCoordinates.get))
    }

    currentFloorPlan
  }

  // Checks which sides of the floor plan that the entry room face, 1 if it has rooms on both sides, 2 if it is in a corner
  private def entrySides[V <: MapArea, E[X] <: UnDiEdge[X]](plan: FloorPlan[V, E], room: RoomArea[V]): Vector[Direction] = {
    var result = Vector[Direction]()

    if (plan.minY == Geom.furthestCoordinate(North, room))
      result = North +: result

    if (plan.minX == Geom.furthestCoordinate(West, room))
      result = West +: result

    if (plan.maxX == Geom.furthestCoordinate(East, room))
      result = East +: result

    if (plan.maxY == Geom.furthestCoordinate(South, room))
      result = South +: result

    result
  }

}
