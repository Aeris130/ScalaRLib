package net.cyndeline.scalarlib.rldrawing.dungeon.separation

import net.cyndeline.rlcommon.math.geom.Rectangle

/**
  * Removes overlap from a group of rectangles by processing pairs with overlap, and separating both by n/2 units,
  * where n is the overlap along the shortest axis between the rectangles in the pair.
  */
class PairSeparation {

  /**
    * @param rectangles A list of rectangles.
    * @return An updated rectangle list where every rectangle occurs on the same index as in the input, and no pair
    *         of rectangles share more than a single coordinate along either axis.
    */
  def separate(rectangles: Vector[Rectangle]): Vector[Rectangle] = {
    var movement = RectangleMovement(rectangles, false)
    val n = rectangles.length

    var finished = true

    do {
      finished = true
      var i = 0
      while (i < n) {
        val rectangle = movement.rectangles(i)
        val overlap = movement.overlap(rectangle).filterNot(_ == i).iterator // Neighbors overlapping R

        while (overlap.hasNext) {
          val n = overlap.next()
          val nr = movement.rectangles(n)

          val intersection = rectangle.intersection(nr)
          if (intersection.isDefined && intersection.get.width > 1 && intersection.get.height > 1) { // Possible a previous neighbor in 'overlap caused R to move away from N
            val moveHorizontally = intersection.get.width <= intersection.get.height
            val distance = if (moveHorizontally) intersection.get.width else intersection.get.height

            val rCenter = center(rectangle, moveHorizontally)
            val nCenter = center(nr, moveHorizontally)

            val rDirection = if (rCenter <= nCenter) degreeLower(moveHorizontally) else degreeUpper(moveHorizontally)
            val nDirection = if (nCenter < rCenter) degreeLower(moveHorizontally) else degreeUpper(moveHorizontally)

            // If the movement distance cannot be evenly split between the pair, move R an additional coordinate
            val rDistance = if (distance % 2 == 0) distance / 2 else (distance / 2) + 1
            val nDistance = distance / 2

            /* Always returns a new manager since overlap doesn't cause movement to fail. */
            movement = movement
              .move(i, rDirection, rDistance).get
              .move(n, nDirection, nDistance).get

            finished = false
          }
        }

        i += 1
      }

    } while (!finished)

    movement.rectangles
  }

  private def center(r: Rectangle, xAxis: Boolean): Int = {
    if (xAxis) r.start.x + (r.width / 2) else r.start.y + (r.height / 2)
  }

  /*
   * Given that degree 0 points to the right, returns the degree that, if moving a rectangle in it, causes the rectangle
   * to move towards negative coordinates on a graph where the y axis increases upwards.
   */
  private def degreeLower(xAxis: Boolean): Double = if (xAxis) 180 /* Left */ else 270 /* Down */
  private def degreeUpper(xAxis: Boolean): Double = if (xAxis) 0 /* Right */ else 90 /* Up */

}
