package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.help.ConnectionBoundary
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.AdjustableRectangle

/**
 * Computes the coordinates and produces a corridor segment that connects two areas (either rooms or corridor bends)
 * such that the segment is positioned in the middle of the areas sides.
 *
 * @constructor Constructs a new corridor segment area factory.
 */
class CorridorSegmentAreaFactory {

  /**
   * Computes a single corridor segment.
 *
   * @param area1 A area to connect the segment to.
   * @param room1ConnectionSide The side of room 1 that the segment connects to.
   * @param area2 The second area to connect to, on the side opposite to 'room1ConnectionSide.
   * @param width Width of the resulting corridor segment. Must be less than or equal to the smallest of the area
   *              sides the segment should be connected to.
   * @return A corridor segment starting at room1 and ending at room2.
   */
  def makeSegment(area1: AdjustableRectangle, room1ConnectionSide: Direction, area2: AdjustableRectangle, width: Int): AdjustableRectangle = {
    if (width < 1) throw new Error("Corridor width must be higher than 0, currently " + width + ".")

    /* If the connection side is North/West, it means that area 2 is above or to the left of area 1. Rather than making
     * cases for all 4 directions, it's easier to just switch places on both areas and reverse the direction, and
     * only include cases for West/South.
     */
    room1ConnectionSide match {
      case North | West => return makeSegment(area2, room1ConnectionSide.opposite, area1, width)
      case _ => {}
    }

    /* Begin by computing a corridor area that lies between the two rooms, but not necessarily in the middle of the
     * connecting side.
     */
    val segmentStartX = room1ConnectionSide match {
      case East => area1.coordinatesOnSide(room1ConnectionSide.parallel)._2
      case South => Math.max(area1.coordinatesOnSide(room1ConnectionSide)._1, area2.coordinatesOnSide(room1ConnectionSide)._1)
    }

    val segmentStartY = room1ConnectionSide match {
      case East => Math.max(area1.coordinatesOnSide(room1ConnectionSide)._1, area2.coordinatesOnSide(room1ConnectionSide)._1)
      case South => area1.coordinatesOnSide(room1ConnectionSide.parallel)._2
    }

    val segmentStopX = room1ConnectionSide match {
      case East => area2.coordinatesOnSide(room1ConnectionSide.parallel)._1
      case South => segmentStartX + width - 1 // Subtract 1 to make it inclusive
    }

    val segmentStopY = room1ConnectionSide match {
      case East => segmentStartY + width - 1 // Subtract 1 to make it inclusive
      case South => area2.coordinatesOnSide(room1ConnectionSide.parallel)._1
    }

    val corridorArea = AdjustableRectangle(Point(segmentStartX, segmentStartY), Point(segmentStopX, segmentStopY))

    /* Use the boundary computation to compute absolute coordinates that the corridor must lie between if the margin of
     * deviation is 0 (i.e where the middle is). To ensure that the corridor ends up within the boundaries of both areas
     * even though only one is used for the computation, use the one with the smallest size along the axis being used.
     */
    val area1LargerThan2 = area1.lengthOfSide(room1ConnectionSide) < area2.lengthOfSide(room1ConnectionSide)
    val area = if (area1LargerThan2) area1 else area2
    val side = if (area1LargerThan2) room1ConnectionSide else room1ConnectionSide.opposite

    val boundaryAlg = new ConnectionBoundary(area, 0)
    val startStop = boundaryAlg.computeConnectionBoundary(corridorArea, side)
    val start = startStop._1

    val segmentStart = room1ConnectionSide match {
      case East => segmentStartY
      case South => segmentStartX
    }

    val mod = if (segmentStart > start) -(segmentStart - start)
    else if (segmentStart < start) start - segmentStart
    else 0

    val middleStartX = room1ConnectionSide match {
      case East => segmentStartX
      case South => segmentStartX + mod
    }

    val middleStartY = room1ConnectionSide match {
      case East => segmentStartY + mod
      case South => segmentStartY
    }

    val middleStopX = room1ConnectionSide match {
      case East => segmentStopX
      case South => segmentStopX + mod
    }

    val middleStopY = room1ConnectionSide match {
      case East => segmentStopY + mod
      case South => segmentStopY
    }

    AdjustableRectangle(Point(middleStartX, middleStartY), Point(middleStopX, middleStopY))
  }
}
