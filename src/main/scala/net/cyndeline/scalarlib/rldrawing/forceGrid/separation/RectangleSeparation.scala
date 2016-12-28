package net.cyndeline.scalarlib.rldrawing.forceGrid.separation

import net.cyndeline.rlcommon.collections.SpatialMultiMap
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Separates a number of overlapping rectangles using the following method:
  *
  *  1. Retrieve all overlapping neighbors of a rectangle r.
  *  1. Find the neighbor n with the shortest overlapping axis.
  *  1. Adjust the position of r along that axis such that r no longer intersects n, but still shares its border.
  *
  * Moving the largest distance needed in order to make r not intersect any of its overlapping neighbors would
  * cause the algorithm to reach its final state sooner. The reason for only moving the shortest distance is so that
  * disproportionally large rectangles won't end up in the center due to the exodus of small rectangles being
  * pushed out.
  *
  * The user may specify a ratio and a number of iterations. If the separation algorithm produces output that
  * breaks the ratio, it will make another iteration attempt by initially spacing the rooms closer to the specified
  * ratio. If no valid solution is found, the best one is output.
  */
class RectangleSeparation() {

  /**
    * @param dimensions Dimensions of rectangles to separate.
    * @return A list of rectangles, where each rectangles index corresponds to the index of the input dimension.
    *         Rectangles will not overlap, but may still share borders with other rectangles.
    */
  def separate(dimensions: Vector[Dimensions], seed: Int): Vector[Rectangle] = {
    if (dimensions.isEmpty)
      return Vector()

    val random = new Random(seed)
//    val totalArea = dimensions.map(d => d.width * d.height).sum
    val n = dimensions.size
//    val averageSideLength = dimensions.map(d => Math.max(d.width, d.height)).sum / n.toDouble
//    val rectangles = new ArrayBuffer[Rectangle]()
//
//    /* The initial rectangle centres are spawned within an ellipse with width and height equal to the square root
//     * of the sum of all rectangle areas.
//     */
//    val axisLength = Math.floor(Math.sqrt(totalArea)).toInt
//    var i = 0
//    while (i < n) {
//      val d = dimensions(i)
//      val p = getRandomPointInEllipse(axisLength, axisLength, random)
//      val r = Rectangle.centerAround(p, d)
//      rectangles += r
//      i += 1
//    }

    val averageShortestSide = dimensions.map(d => Math.min(d.width, d.height)).sum / n

    val walk = new RandomWalk(Point(0, 0), averageShortestSide * 10, n).walk(random).zipWithIndex
    val rectangles = walk.map(p => Rectangle(p._1, dimensions(p._2)))

    val pairSep = new PairSeparation()
    pairSep.separate(rectangles)

//    var movementManager = RectangleMovement(rectangles.toVector, preventOverlap = true)
//
//    /* Main algorithm starts here. Move each rectangle outwards from the center (relative to the angle that the
//     * rectangle has to the center) until it doesn't overlap other rectangles any longer. Repeat for all rectangles.
//     */
//    val center = Point(0, 0) // Due to how the ellipse points are generated
//
//    /* Since pushing every rectangle outwards results in a rough circle, the maximum movement is set to half the radius
//     * that a circle would have, given the total area. This is to prevent rectangles from being placed too far away from
//     * the cluster due to the doubling of the movement distance.
//     */
//    val maxDistance = Math.ceil(Math.sqrt(totalArea / Math.PI) / 2d)
//    var j = 0
//    while (j < n) {
//      val rectangle = movementManager.rectangles(j)
//      val angle = center.angleTo(rectangle.start)
//      var moveDistance = averageSideLength
//
//      var done = false
//
//      if (!movementManager.isClear(j)) {
//        while (!done) {
//          val moved = movementManager.move(j, angle, moveDistance)
//
//          if (moved.isDefined) {
//            movementManager = moved.get
//            done = true
//
//          } else {
//            moveDistance += Math.min(maxDistance, moveDistance * 2)
//          }
//        }
//      }
//
//      j += 1
//    }
//
//    movementManager.rectangles
  }

  private def getRandomPointInEllipse(ellipseWidth: Int, ellipseHeight: Int, random: Random): Point = {
    val t = 2 * Math.PI * random.nextDouble()
    val u = random.nextDouble() + random.nextDouble()
    var r = 0d
    if (u > 1)
      r = 2 - u
    else
      r = u
    val x = ellipseWidth * r * Math.cos(t) / 2d
    val y = ellipseHeight * r * Math.sin(t) / 2d
    Point(Math.floor(x).toInt, Math.floor(y).toInt)
  }

}
