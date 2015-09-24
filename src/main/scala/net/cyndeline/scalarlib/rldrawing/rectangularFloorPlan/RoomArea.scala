package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan

import net.cyndeline.rlcommon.util.{Point, RectangleCoordinates}
import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom

/**
 * An area to be drawn in a rectangular drawing. Construct using the accompanying factory object.
 *
 * @param original The room object originally supplied by the user to be parsed into an actual map room, None if this
 *                 is a gate resulting from an edge split.
 * @param gate The original vertices connected to this gate, or None if this is a regular area.
 */
class RoomArea[V] private (original: Option[V], gate: Option[(V, V)], override val start: Point, override val stop: Point) extends DrawnRoom[V] with RectangleCoordinates {
  require(stop.x - start.x > 0 || stop.y - start.y > 0, this  + "Cannot cover only a single coordinate: " + start + " to " + stop)

  /** True if this area is not based on one of the original vertices in the graph, but instead was added to make the
    * graph valid for dualization.
    */
  val isGate: Boolean = !original.isDefined

  /** True if this room area belongs to one of the original vertices in the graph. */
  val isRoom: Boolean = original.isDefined

  /**
   * @return The original vertices connected to this gate. Throws an error if this area represents a regular vertex.
   */
  def gateNeighbors: (V, V) = gate.getOrElse(throw new Error("Cannot retrieve gate neighbors from an original area."))

  /**
   * Sets a new pair of start/stop coordinates for this room.
   * @param newStart New start point.
   * @param newStop New stop point.
   * @return A copy of this room using the new start and stop point.
   */
  def withNewCoordinates(newStart: Point, newStop: Point): RoomArea[V] = new RoomArea(original, gate, newStart, newStop)

  /**
   * @return The room object originally supplied by the user to be parsed into an actual map room. Throws an error
   *         if attempting to retrieve a room from a gate.
   */
  override def originalRoom: V = original.getOrElse(throw new Error("Cannot retrieve the original room from an area representing a split."))

  /**
   * @return A rectangular area represented by a start and stop coordinate (both inclusive).
   */
  override def area: RectangleCoordinates = this

  override val toString: String = {
    val builder = new StringBuilder()
    if (isRoom)
      builder ++= "Room | " + originalRoom
    else
      builder ++= "Gate | " + gateNeighbors

    builder ++= " | from " + start + " to " + stop + "/"


    builder.toString()
  }

  override def equals(other: Any): Boolean = other match {
    case ra: RoomArea[V] => {
      if (isRoom != ra.isRoom || isGate != ra.isGate) {
        false
      } else {
        if (start != ra.start && stop != ra.stop) {
          false
        } else if (isGate) {
          gateNeighbors == ra.gateNeighbors
        } else {
          originalRoom == ra.originalRoom
        }
      }

    }
    case _ => false
  }

  override val hashCode: Int = original.## ^ gate.## ^ start.## ^ stop.##
}

/**
 * Factory object used for instantiation.
 */
object RoomArea {

  /**
   * Constructs an area based on an original vertex in the graph.
   */
  def apply[V](original: V, start: Point, stop: Point) = new RoomArea(Some(original), None, start, stop)

  /**
   * Constructs an area based on a split, resulting in a gate area.
   */
  def apply[V](gateNeighbor1: V, gateNeighbor2: V, start: Point, stop: Point) = new RoomArea[V](None, Some((gateNeighbor1, gateNeighbor2)), start, stop)

}
