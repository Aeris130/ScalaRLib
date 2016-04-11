package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.rlcommon.math.geom.Point

import scala.collection.mutable.ListBuffer

/**
 * To guarantee that every area in a rectangular layout reaches its minimum size, it may be necessary to scale the
 * entire drawing such that every room is equal or higher than its target area. In doing so, it is also desirable to
 * keep the aspect ratios of each room as uniform as possible, with the optimum being that every room has a 1:1 ratio
 * (not achievable).
 *
 * This can be formulated as an optimization problem: Given a set of rectangular areas, find the minimal two values
 * xi, yi such that increasing the width of every rectangle by xi and the height of every rectangle by yi results in
 * every rectangle reaching its target area, and the sum of aspect ratios R=>Max(w/h, h/w) is minimized.
 *
 * As this cannot be expressed using linear constraints, we instead apply a heuristic by noting the following property
 * of rectangular areas: Given a rectangle whose width and height differs, an increase of length in the lowest side
 * results in a greater increase in total area than an increase in the larger side. Doing so also reduces the aspect
 * ratio, rather than increasing it.
 *
 * Thus, every rectangle that has yet to meet its target area is divided into two sets W and H: W containing rectangles
 * that are wider than tall, and vice versa for H. Uniform rectangles go into a set U. The counter T keeps track of the
 * number of rectangles that has not yet achieved their target area. The counter X keeps track of the increase in width,
 * and Y keeps track of height.
 *
 *  1. While T > 0, select the set W/H that has the highest number of rectangles in it.
 *   1. If that axis is W, increase the width of every rectangle by 1, otherwise increase the height by 1. Increase
 *   X or Y depending on which side that was increased.
 *    1. If increasing the height/width causes the rectangles ratio to shift, move it to the appropriate set, (W, H, U).
 *   1. If the rectangle meets its target area and did not previously do so, decrease T by 1.
 *
 * By always increasing the length of the axis that has the most rectangles in it, every increase results in a maximum
 * decrease of aspect ratio.
 *
 * @param minimumLength The smallest height/width permitted in any rectangle. Every rectangle will be scaled to
 *                      meet this requirement before taking aspect ratios into consideration.
 */
class RoomSizeIncreaseAlg(minimumLength: Int) {
  require(minimumLength >= 1, "Minimum length must be 1 or higher.")

  /**
   * @param rectangles Start and stop coordinates as well as the target area of every rectangle.
   * @return The amount that x and y needs to increase by in order for every rectangle to hit its target area.
   */
  def computeIncrease(rectangles: Vector[((Point, Point), Int)]): (Int, Int) = {
    var x = 0
    var y = 0
    var w = 0
    var h = 0
    var u = 0
    val allRectangles = new ListBuffer[MutableRectangle]()

    for (r <- rectangles) {
      val coordinates = r._1
      val targetArea = r._2
      val rectangle = new MutableRectangle(coordinates._1, coordinates._2, targetArea)
      allRectangles += rectangle
    }

    val minIncrease = computeMinimumIncrease(allRectangles)
    x = minIncrease._1
    y = minIncrease._2
    allRectangles.foreach(_.increaseWidth(x))
    allRectangles.foreach(_.increaseHeight(y))

    val initialValues = computeSetNumbers(allRectangles)
    w = initialValues._1
    h = initialValues._2
    u = initialValues._3

    var t = allRectangles.count(r => r.height * r.width < r.target)

    while (t > 0) {
      if (w > h) {
        x += 1
        allRectangles.foreach(_.increaseHeight(1))
      } else if (h > w) {
        y += 1
        allRectangles.foreach(_.increaseWidth(1))
      } else {
        x += 1
        allRectangles.foreach(_.increaseWidth(1)) // Most monitors are wider then tall, so...
      }

      w = 0
      h = 0
      u = 0
      t = 0
      for (r <- allRectangles) {
        if (r.height * r.width < r.target)
          t += 1

        if (r.width > r.height)
          w += 1
        else if (r.height > r.width)
          h += 1
        else
          u += 1
      }
    }

    (x, y)
  }

  private def computeMinimumIncrease(rs: ListBuffer[MutableRectangle]): (Int, Int) = {
    var biggestDifferenceX = 0
    var biggestDifferenceY = 0

    for (r <- rs) {
      if (r.width < minimumLength && minimumLength - r.width > biggestDifferenceX)
        biggestDifferenceX = minimumLength - r.width

      if (r.height < minimumLength && minimumLength - r.height > biggestDifferenceY)
        biggestDifferenceY = minimumLength - r.height
    }

    (biggestDifferenceX, biggestDifferenceY)
  }

  private def computeSetNumbers(rs: ListBuffer[MutableRectangle]): (Int, Int, Int) = {
    var w = 0
    var h = 0
    var u = 0
    for (r <- rs) {
      if (r.width > r.height)
        w += 1
      else if (r.height > r.width)
        h += 1
      else
        u += 1
    }

    (w, h, u)
  }

  private class MutableRectangle(start: Point, stop: Point, val target: Int) {
    private var x = 0
    private var y = 0

    def increaseHeight(m: Int) { y += m }
    def increaseWidth(m: Int) { x += m }
    def meetsTarget: Boolean = width * height >= target

    def width = stop.x - start.x + 1 + x
    def height = stop.y - start.y + 1 + y
  }
}
