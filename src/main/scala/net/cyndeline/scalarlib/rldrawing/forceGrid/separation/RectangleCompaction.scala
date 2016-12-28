package net.cyndeline.scalarlib.rldrawing.forceGrid.separation

import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.scalarlib.rldrawing.common._

import scala.collection.mutable.ArrayBuffer

/**
  * Moves a number of rectangles towards a target, and stops when none of the rectangles can move further.
  */
class RectangleCompaction {

  /**
    * @param target Coordinate that all disjoint rectangles should attempt to place in their center.
    * @param areas A list of rectangles, where each rectangles index corresponds to the index of the rectangle in the
    *              input vector.
    * @param compact Indices of all rectangles to compact towards target.
    * @return The input rectangle list where all disjoint rectangles have been moved as close to the target as possible.
    */
  def compact(target: Point, areas: Vector[Rectangle], compact: List[Int]): Vector[Rectangle] = {
    //TODO rebalance after n modifications
    var kdTree = KDTree.rectangleTree(areas)
    val allRectangles = new ArrayBuffer[Rectangle]()
    allRectangles ++= areas

    def move(id: Int, r: Rectangle, to: Rectangle): Unit = {
      kdTree = kdTree.delete(r)
      kdTree = kdTree.insert(to)
      allRectangles(id) = to
    }
    def isEmpty(area: Rectangle): Boolean = {
      val overlapping = kdTree.rangeSearch(area)
      !overlapping.exists(r => {
        val i = r.intersection(area).get
        i.height > 1 && i.width > 1
      })
    }

    /* Start by moving every rectangle in an angle towards the target. */
    var noMovementPossible = true
    do {

      val compactIds = compact.iterator
      while (compactIds.hasNext) {
        val id = compactIds.next()
        val initialRectangle = allRectangles(id)
        val angle = target.angleTo(initialRectangle.start)
        var latest = initialRectangle
        var doneMoving = false
        val distance = Math.max(initialRectangle.width, initialRectangle.height)

        do {
          val moved = latest.start.move(angle, distance)
          val updatedRect = Rectangle(moved, initialRectangle.width, initialRectangle.height)

          // Always check distance to target to prevent an area from moving back and forth around it
          if (moved.distanceTo(target) < latest.start.distanceTo(target)) {
            if (!isEmpty(updatedRect)) {
              doneMoving = true
            } else {
              latest = updatedRect
            }
          }

        } while (!doneMoving)

        if (latest != initialRectangle) {
          noMovementPossible = false
          move(id, initialRectangle, latest)
        }
      }

    } while (!noMovementPossible)

    /* When none of the rectangles can move further, adjust them a fixed number of coordinates
     * until no rectangle can be moved.
     */
    noMovementPossible = true

    do {

      val compactIds = compact.iterator
      while (compactIds.hasNext) {
        val id = compactIds.next()
        val initialRectangle = allRectangles(id)
        val latest = initialRectangle

        /* Try moving the rectangle diagonally towards the target. If that doesn't work, move it
         * horizontally or vertically instead.
         */

      }

    } while (!noMovementPossible)



//
//    def inDirection(d: Direction, r: Rectangle): Boolean = d match {
//      case North => target.y > DirectionProperty.coordinate(r, d)
//      case West => target.x < DirectionProperty.coordinate(r, d)
//      case East => target.x > DirectionProperty.coordinate(r, d)
//      case South => target.y < DirectionProperty.coordinate(r, d)
//    }
//
//    // Iterate over the rectangle set until none of them can move further
//    var packed = true
//    do {
//      val moving = toMove.iterator
//      while (moving.hasNext) {
//        val area = moving.next()
//        var done = false
//
//        do {
//          if (!area.containsPoint(target)) {
//
//            /* Search in an area twice the size of the rectangle to move, then move it as far towards the target
//             * along both axises that it can without intersecting another rectangle.
//             */
//
//
//
//            area + (1, 2)
//          } else {
//            done = true
//          }
//        } while (!done)
//
//      }
//    } while (!packed)


    allRectangles.toVector
  }

}
