package net.cyndeline.scalarlib.rldrawing.util

import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rldrawing.common.RectangleCoordinates

/**
 * Commonly used methods for geometry. Used to allow changes in inclusive/exclusive coordinate ranges without having
 * to track down class-specific code.
 */
object Geom {

  def width(start: Int, stop: Int): Int = {
    validate(start, stop)
    stop - start + 1
  }
  def width(start: Point, stop: Point): Int = {
    width(start.x, stop.x)
  }
  def width(r: RectangleCoordinates): Int = width(r.start, r.stop)
  def height(start: Point, stop: Point): Int = {
    height(start.y, stop.y)
  }
  def height(start: Int, stop: Int): Int = {
    validate(start, stop)
    stop - start + 1
  }
  def height(r: RectangleCoordinates): Int = height(r.start, r.stop)

  def area(startX: Int, stopX: Int, startY: Int, stopY: Int): Int = width(startX, stopX) * height(startY, stopY)
  def area(start: Point, stop: Point): Int = area(start.x, stop.x, start.y, stop.y)
  def area(r: RectangleCoordinates): Int = area(r.start.x, r.stop.x, r.start.y, r.stop.y)

  def areaCoordinates(startX: Int, startY: Int, width: Int, height: Int): (Point, Point) = {
    (Point(startX, startY), Point(startX + width - 1, startY + height - 1))
  }

  def areaCoordinates(startXAndY: Int, width: Int, height: Int): (Point, Point) = areaCoordinates(startXAndY, startXAndY, width, height)

  /** Computes the last inclusive coordinate. */
  def furthestCoordinate(direction: Direction, start: Point, stop: Point): Int = direction match {
    case North => start.y
    case West => start.x
    case South => stop.y
    case East => stop.x
  }
  def furthestCoordinate(direction: Direction, r: RectangleCoordinates): Int = furthestCoordinate(direction, r.start, r.stop)

  private def validate(start: Int, stop: Int) {
    require(start <= stop, "start (" + start + ") must be <= stop (" + stop + ")")
  }
}
