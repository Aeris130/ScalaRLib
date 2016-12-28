package net.cyndeline.scalarlib.rldrawing.common

import net.cyndeline.rlcommon.math.geom.Rectangle

/**
  * Parses properties from shapes depending on which direction they're being parsed from.
  */
object DirectionProperty {

  /**
    * Start- and stop coordinates for a side S. If S is west, the coordinates will be (start.y, stop.y).
    * @param r A rectangle.
    * @param s A direction.
    */
  def interval(r: Rectangle, s: Direction): (Int, Int) = s match {
    case North | South => (r.start.x, r.stop.x)
    case West | East => (r.start.y, r.stop.y)
  }

  /**
    * @param r A rectangle R.
    * @param s A side of N.
    * @return The coordinate of R closest to S on the axis perpendicular to S. If S is West, then the starting x
    *         coordinate of R is returned. If S is North, the stop y coordinate is returned instead.
    */
  def coordinate(r: Rectangle, s: Direction): Int = s match {
    case North => r.stop.y
    case West => r.start.x
    case South => r.start.y
    case East => r.stop.x
  }

}
