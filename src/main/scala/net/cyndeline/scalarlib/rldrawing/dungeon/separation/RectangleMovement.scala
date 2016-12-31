package net.cyndeline.scalarlib.rldrawing.dungeon.separation

import net.cyndeline.rlcommon.collections.SpatialMultiMap
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}

/**
  * Manages and adjusts the positions of a set of rectangles.
  *
  * @param rMap Maps rectangular areas to the id of the rectangles occupying it. Needed since multiple rectangles may
  *             have the exact same dimensions and starting coordinate.
  */
class RectangleMovement private (val rectangles: Vector[Rectangle], rMap: SpatialMultiMap[Rectangle, Int, Rectangle], preventOverlap: Boolean) {

  /**
    * Adjusts the position of a rectangle.
    * @param id The index of a rectangle in the rectangles vector.
    * @param angle Angle to move the rectangle in between 0 and 360. 0 moves the rectangle right.
    * @param distance Number of coordinates to move rectangle in.
    * @return An updated copy of the manager if movement was successful (always the case if overlap prevention is
    *         turned off), otherwise None.
    */
  def move(id: Int, angle: Double, distance: Double): Option[RectangleMovement] = {
    performMovement(id, angle, distance, None)
  }

  /**
    * Adjusts the position of a rectangle with a given angle and distance, but only if the starting coordinate of
    * the resulting rectangles lies closer to the target than before.
    * @param target Target coordinate to move towards.
    * @return An updated copy of the manager if movement was successful (always the case if overlap prevention is
    *         turned off), otherwise None.
    */
  def moveTowardsTarget(id: Int, angle: Double, distance: Double, target: Point): Option[RectangleMovement] = {
    performMovement(id, angle, distance, Some(target))
  }

  /**
    * @param area A rectangular area.
    * @return The id of all rectangles overlapping the area.
    */
  def overlap(area: Rectangle): Set[Int] = rMap.getRange(area)

  /**
    * @param id Index of a rectangle.
    * @return True if no other rectangle overlaps R by more than a single coordinate along either axis.
    */
  def isClear(id: Int): Boolean = !overlaps(rectangles(id), id)

  private def performMovement(id: Int, angle: Double, distance: Double, target: Option[Point]): Option[RectangleMovement] = {
    var currentRectangles = rectangles
    val r = currentRectangles(id)
    val newR = moveRectangle(r, angle, distance)

    if (target.isEmpty || newR.start.distanceTo(target.get) < r.start.distanceTo(target.get)) {
      val updatedMap = updateMap(id, r, newR).getOrElse(return None) // None if overlap prevention is active and fails
      currentRectangles = currentRectangles.updated(id, newR)
      Some(new RectangleMovement(currentRectangles, updatedMap, preventOverlap))
    } else {
      None
    }
  }

  private def moveRectangle(r: Rectangle, angle: Double, distance: Double) = {
    Rectangle(r.start.move(angle, distance), r.width, r.height)
  }

  private def updateMap(id: Int, oldR: Rectangle, newR: Rectangle): Option[SpatialMultiMap[Rectangle, Int, Rectangle]] = {
    val withoutR = rMap - (oldR, id)

    if (!preventOverlap || !overlaps(newR, id)) {
      Some(withoutR + (newR, id))
    } else {
      None
    }
  }

  private def overlaps(r: Rectangle, ignore: Int) = {
    val overlapping = rMap.getRange(r)
    overlapping.exists(id => id != ignore && {
      val other = rectangles(id)
      val i = other.intersection(r)
      i.nonEmpty && (i.get.height > 1 && i.get.width > 1)
    })
  }

}

object RectangleMovement {

  /**
    * @param initialRectangles Initial set of rectangles to adjust. Can overlap each other.
    * @param preventOverlap If set to true, prevents movement where two rectangles end up overlapping by more than one
    *                       coordinate along any axis.
    */
  def apply(initialRectangles: Vector[Rectangle], preventOverlap: Boolean = true) = {
    val n = initialRectangles.size
    var rMap: SpatialMultiMap[Rectangle, Int, Rectangle] = SpatialMultiMap.withRectangles(n)
    var i = 0
    while (i < n) {
      rMap = rMap + (initialRectangles(i) -> i)
      i += 1
    }
    new RectangleMovement(initialRectangles, rMap, preventOverlap)
  }

}