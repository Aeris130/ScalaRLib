package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.help

import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.RectangularArea

/**
 * Computes how much an area A connected to another area B is allowed to move along some side of B before A breaks
 * the boundary of B and ends up with coordinates outside it.
 *
 * Example: An area A with coordinates from 5 to 8 on the x axis may be connected to another area B with coordinates
 * from 3 to 10. By giving A a boundary of length 0, it is not allowed to move higher or lower than its current
 * position. A deviation of 1 however, would allow it to move between 4 and 9.
 *
 * @constructor Constructs a new boundary computation.
 * @param boundaryArea The area with the boundary, ex: a room or corridor bend.
 * @param maxDeviation The maximum amount of coordinates that an area is allowed to deviate from the center of
 *                     any side of the boundary. Only applies if the start/stop coordinates of a boundary side
 *                     lies outside of this range, otherwise the start/stop is used. To allow connecting areas to move
 *                     freely across the area connected to, set this to any large value (for example, the maximum size
 *                     of the boundary areas sides).
 */
class ConnectionBoundary(boundaryArea: RectangularArea, maxDeviation: Int) {

  /**
   * Creates a connection boundary with 0 deviation.
   * @param boundaryArea The area with the boundary, ex: a room or corridor bend.
   * @return A connection boundary with 0 deviation.
   */
  def this(boundaryArea: RectangularArea) = this(boundaryArea, 0)

  /**
   * Computes the start/stop coordinate along some axis that an area connecting to this boundary must stay inside.
   * These coordinates assumes that the connection area originally was centered in the boundary.
   *
   * @param connectingArea The area connecting to this boundary, ex: a corridor.
   * @param side Which side of the boundary the connecting area connects to.
   * @return A pair of coordinates. The start coordinate of the connecting area must be >= than the first value,
   *         and the stop coordinate must be <= than the second value.
   */
  def computeConnectionBoundary(connectingArea: RectangularArea, side: Direction): (Int, Int) = {
    val connectingAxisCoordinates = connectingArea.coordinatesOnSide(side)
    val boundaryCoordinates = boundaryArea.coordinatesOnSide(side)

    if (connectingAxisCoordinates._1 < boundaryCoordinates._1 || connectingAxisCoordinates._2 > boundaryCoordinates._2)
      throw new Error("The area " + connectingArea + " lies outside the area " + boundaryArea + " on the " + side + " axis.")

    val boundarySize = boundaryArea.lengthOfSide(side)
    val connectionSize = connectingArea.lengthOfSide(side)
    val boundaryStart = boundaryCoordinates._1
    val boundaryStop = boundaryCoordinates._2

    /* If both sizes doesn't have the same modulus, it is assumed that the boundary area is larger than the connecting
     * area (otherwise an exception should be thrown above), making it safe to increase the boundary by 1 to make them
     * both even or odd.
     */
    if (boundarySize % 2 != 0 && connectionSize % 2 != 0) {
      computeBoundariesOnOddSizes(boundaryStart, boundaryStop, boundarySize, connectionSize, false)

    } else if (boundarySize % 2 == 0 && connectionSize % 2 == 0) {
      computeBoundariesOnEvenSizes(boundaryStart, boundaryStop, boundarySize, connectionSize, false)

    } else if (connectionSize % 2 != 0) { // Boundary is even, increase to make it odd
    val result = computeBoundariesOnOddSizes(boundaryStart, boundaryStop, boundarySize, connectionSize, true)
      if (result._2 > boundaryStop) (result._1, boundaryStop) else result

    } else { // boundarySize % 2 != 0 | Boundary is odd, increase to make it even
    val result = computeBoundariesOnEvenSizes(boundaryStart, boundaryStop, boundarySize, connectionSize, true)
      if (result._2 > boundaryStop) (result._1, boundaryStop) else result
    }
  }

  private def computeBoundariesOnEvenSizes(boundaryStart: Int, bStop: Int, bSize: Int, connectionSize: Int, makeBoundaryEven: Boolean): (Int, Int) = {
    val boundaryStop = if (makeBoundaryEven) bStop + 1 else bStop
    val boundarySize = if (makeBoundaryEven) bSize + 1 else bSize
    val boundaryModification = (connectionSize / 2) + maxDeviation
    val startBoundary = boundaryStart + (boundarySize / 2) - boundaryModification
    val stopBoundary = boundaryStop - (boundarySize / 2) + boundaryModification

    val removeAdjustmentStart = if (makeBoundaryEven) startBoundary - 1 else startBoundary
    val removeAdjustmentStop = if (makeBoundaryEven) stopBoundary - 1 else stopBoundary

    val finalStartB = if (removeAdjustmentStart < boundaryStart) boundaryStart else removeAdjustmentStart
    val finalStopB = if (removeAdjustmentStop > boundaryStop) boundaryStop else removeAdjustmentStop

    (finalStartB, finalStopB)
  }

  private def computeBoundariesOnOddSizes(boundaryStart: Int, bStop: Int, bSize: Int, connectionSize: Int, makeBoundaryOdd: Boolean): (Int, Int) = {
    val boundaryStop = if (makeBoundaryOdd) bStop + 1 else bStop
    val boundarySize = if (makeBoundaryOdd) bSize + 1 else bSize

    val boundaryMiddleCoordinate = boundaryStart + Math.floor(boundarySize / 2.0)
    val connectionSizeOutsideMiddle = Math.floor(connectionSize / 2.0)

    // Start/stop after adding any deviation, may exceed the boundary
    val deviationStart = boundaryMiddleCoordinate - connectionSizeOutsideMiddle - maxDeviation
    val deviationStop = boundaryMiddleCoordinate + connectionSizeOutsideMiddle + maxDeviation

    val adjustedStart = if (makeBoundaryOdd) deviationStart - 1 else deviationStart
    val adjustedStop = if (makeBoundaryOdd) deviationStop - 1 else deviationStop

    val startBoundary = Math.max(boundaryStart, adjustedStart).toInt
    val stopBoundary = Math.min(boundaryStop, adjustedStop).toInt

    (startBoundary, stopBoundary)
  }
}
