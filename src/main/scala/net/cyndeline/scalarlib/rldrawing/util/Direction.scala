package net.cyndeline.scalarlib.rldrawing.util

/**
 * Relative directions of 2D grid elements.
 *
 * usage: import Direction._
 */
object Direction extends Enumeration {
  type Direction = Value
  val North, South, West, East = Value

  class DirectionValue(dir: Value) {
    def opposite = dir match {
      case North => South
      case South => North
      case West => East
      case East => West
    }
    def parallel = dir match {
      case North | South => West
      case _ => North
    }
  }

  implicit def value2DirectionValue(suit: Value) = new DirectionValue(suit)
}
