package net.cyndeline.scalarlib.rldrawing.forceGrid.separation

import net.cyndeline.rlcommon.math.geom.Point

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Generates a list of coordinates by walking randomly in 8 directions.
  *
  * @param start Starting point for walks.
  * @param step Number of coordinates to move in each step.
  * @param distance Number of coordinates to produce.
  */
class RandomWalk(start: Point, step: Int, distance: Int) {

  def walk(random: Random): Vector[Point] = {
    val result = new ArrayBuffer[Point]()
    val xAdjust = Array(-1, 0, 1)
    val yAdjust = Array(-1, 0, 1)
    var i = 0
    var current = start
    while (i < distance) {
      current = current + (xAdjust(random.nextInt(3)) * step, yAdjust(random.nextInt(3)) * step)
      result += current
      i += 1
    }

    result.toVector
  }
}
