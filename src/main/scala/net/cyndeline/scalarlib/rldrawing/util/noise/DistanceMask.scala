package net.cyndeline.scalarlib.rldrawing.util.noise

import net.cyndeline.rlcommon.math.Normalize
import net.cyndeline.rlcommon.util.Matrix2DOp
import net.cyndeline.scalarlib.rldrawing.util.noise.DistanceMask._

/**
  * Changes the noise value e for a grid of values such that e gets lower the further towards the grids edges it is
  * positioned. The mask uses two different formulas for distance (manhattan rectangles and euclidean ellipses), and two
  * formulas for modifying e (addition and multiplication).
  *
  * Manhattan distance d: 2 * max(abs(nx), abs(ny))
  * Euclidean distance d: 2 * sqrt(nx*nx + ny*ny)
  * Addition: e = e + a - b * pow(d, c)
  * Multiplication: e = (e + a) * (1 - b * pow(d, c))
  *
  * The implementation is based on the island section in http://www.redblobgames.com/maps/terrain-from-noise/.
  *
  * @param a Pushes all values up.
  * @param b Pushes all values down.
  * @param c Controls how fast the drop-off is for the value modification.
  * @param distance Determines which distance function should be used.
  */
class DistanceMask private (a: Double, b: Double, c: Double, distance: DistanceFunc, mask: MaskFunc) extends NoiseMask2D {

  def withManhattanDistance = new DistanceMask(a, b, c, ManhattanDistance, mask)
  def withEuclideanDistance = new DistanceMask(a, b, c, EuclideanDistance, mask)

  def withAddition = new DistanceMask(a, b, c, distance, Add)
  def withMultiplication = new DistanceMask(a, b, c, distance, Multiply)

  /**
    * @param noiseGrid Matrix with noise values to apply mask onto. Every value should be in the range [0, 1].
    * @return A copy of the noise grid with the mask applied. See class description.
    */
  def applyMask(noiseGrid: Array[Array[Double]]): Array[Array[Double]] = {
    require(noiseGrid.length > 0, "Cannot apply mask to empty grid.")
    val width = noiseGrid.length
    val height = noiseGrid(0).length
    val centerX = Math.floor(width / 2d).toInt
    val centerY = Math.floor(height / 2d).toInt

    val distanceFunction = distance match {
      case ManhattanDistance => manhattan _
      case EuclideanDistance =>
        val maxNx = Math.ceil(width / 2d)
        val maxNy = Math.ceil(height / 2d)
        val maxSqrt = 2 * Math.sqrt(maxNx * maxNx + maxNy * maxNy)
        euclidean(Math.sqrt(2)) _
    }
    def maskFunction = mask match {
      case Add => add _
      case Multiply => multiply _
    }

    var lowest = Double.MaxValue
    var highest = Double.MinValue
    val finalGrid = Array.ofDim[Double](width, height)

    def applyMask(v: Double, x: Int, y: Int): Double = {
      val d = distanceFunction(x, y, centerX, centerY, width, height)
      val value = maskFunction(noiseGrid(x)(y), d)
      if (value < lowest) lowest = value
      if (value > highest) highest = value
      value
    }
    def normalizeFinalGrid(v: Double, x: Int, y: Int): Double = {
      Normalize(v, lowest, highest)
    }

    Matrix2DOp.modify(finalGrid, applyMask _)
    Matrix2DOp.modify(finalGrid, normalizeFinalGrid _)
    finalGrid
  }

  private def manhattan(x: Int, y: Int, centerX: Int, centerY: Int, width: Int, height: Int): Double = {
    Normalize(2 * Math.max(Math.abs(x - centerX), Math.abs(y - centerY)), 0, Math.max(width, height))
  }

  private def euclidean(max: Double)(x: Int, y: Int, centerX: Int, centerY: Int, width: Int, height: Int): Double = {
    val nx = Math.abs(x - centerX)
    val ny = Math.abs(y - centerY)
    val normX = Normalize(nx, 0, width / 2d)
    val normY = Normalize(ny, 0, height / 2d)
    val d = Math.sqrt(normX * normX + normY * normY)
    Normalize(d, 0, max)
  }

  private def add(value: Double, distance: Double) = value + a - (b * Math.pow(distance, c))
  private def multiply(value: Double, distance: Double) = (value + a) * (1 - (b * Math.pow(distance, c)))

}

object DistanceMask {

  private sealed trait DistanceFunc
  private case object ManhattanDistance extends DistanceFunc
  private case object EuclideanDistance extends DistanceFunc

  private sealed trait MaskFunc
  private case object Add extends MaskFunc
  private case object Multiply extends MaskFunc

  def apply(a: Double, b: Double, c: Double) = new DistanceMask(a, b, c, ManhattanDistance, Add)

}
