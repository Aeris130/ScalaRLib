package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan

import net.cyndeline.scalarlib.rldrawing.util.Direction.Direction
import net.cyndeline.scalarlib.rldrawing.util.{Connection, Intersection, Point}

/**
 * Specifies a neighbor for a room in a rectangular floor plan, and how to draw it relative to the owner.
 *
 * @param direction The direction in which the neighbor is located relative to the owner. If the direction is south,
 *                  it means the neighbor lies south of the owner.
 * @param neighbor The area of the neighbor.
 * @param connection The intersecting coordinates between the owner of this intersection and its neighbor.
 */
case class FloorIntersection[VType](direction: Direction, neighbor: RoomArea[VType], connection: Connection) {

}
