package net.cyndeline.scalarlib.rldrawing.dungeon.separation

import net.cyndeline.rlcommon.math.geom.{Centroid, Dimensions, Point, Rectangle}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Heuristic that looks for an empty space as close as possible to the center of a map in order to place
  * rectangular areas there. The algorithm runtime is dependent on the number of coordinates in the map,
  * which is (1.5n x 1.5n), where n is the sum of the largest sides of all the input rectangles (this is to ensure
  * that every rectangle can be placed before running out of space).
  *
  * Area placement is done in a spiral starting at the maps center, working itself outwards.
  */
class RectanglePlacement {

  /**
    * @param areas A list of rectangle dimensions.
    * @return A list where every index corresponds to the initial dimension list. None if the algorithm failed to place
    *         every area.
    */
  def computePlacement(areas: Vector[Dimensions]): Option[Vector[Rectangle]] = {
    if (areas.isEmpty) { return Some(Vector()) }
    val al = areas.length
    val r = new Random(al)

    require(!areas.exists(d => d.width < 3 || d.height < 3), "All areas must have width and height 3 or more.")

    val size = computeSize(areas)
    val grid = PlacementGrid(size, areas.map(d => Math.max(d.width, d.height)).max)
    val centerValue = Math.ceil(size / 2d).toInt
    val center = Point(centerValue, centerValue) // Always exactly center since the size is always odd
    val finalAreas = new ArrayBuffer[Rectangle]()
    finalAreas ++= areas.map(_ => null)

    var i = 0
    while (i < al) {
      if (!spiralPlace(center, areas(i), i, grid, finalAreas, r))
        return None

      i += 1
    }

    val areaVector = finalAreas.toVector
    val newCenter = if (areas.length > 1) Centroid.fromPoints(areaVector.map(_.center)) else center

    Some(adjustTowardsCenter(newCenter, areaVector))
  }

  /** @return The sum of the longest sides of all rectangles to be placed. +1 if needed to make the grid have
    *         odd length.
    */
  private def computeSize(dims: Vector[Dimensions]): Int = {
    val totalArea = dims.map(d => Math.pow(Math.max(d.width, d.height), 2)).sum
    val side = Math.sqrt(totalArea).toInt * 2
    if (side % 2 == 0)
      side + 1
    else
      side
  }

  /** The algorithm doesn't really spiral, it just moves along the border of a square that expands by 2 along both
    * axises every iteration.
    */
  private def spiralPlace(center: Point,
                          area: Dimensions,
                          index: Int,
                          grid: PlacementGrid,
                          result: ArrayBuffer[Rectangle],
                          randomizer: Random): Boolean = {
    var level = 0

    def place(fromX: Int, toX: Int, fromY: Int, toY: Int): Boolean = {
      val xit = randomizer.shuffle(fromX to toX) // Keeps areas from being skewed towards corners
      val yit = randomizer.shuffle(fromY to toY)
      for (x <- xit; y <- yit)
        if (grid.canBePlaced(area, x, y)) {
          val r = Rectangle(Point(x, y), area)
          grid.place(r)
          result.update(index, r)
          return true
        }

      false
    }

    while (center.x - level >= 0) { // Grid is an odd square, so no need to check y
      val startX = center.x - level
      val startY = center.y - level
      val stopX = center.x + level
      val stopY = center.y + level

      if (place(startX, startX, startY, stopY)) // Left
        return true

      if (place(stopX, stopX, startY, stopY)) // Right
        return true

      if (place(startX, stopX, startY, startY)) // Bottom
        return true

      if (place(startX, stopX, stopY, stopY)) // Top
        return true

      level += 1
    }

    false
  }

  private def adjustTowardsCenter(center: Point, rectangles: Vector[Rectangle]): Vector[Rectangle] = {
    var movementManager = RectangleMovement(rectangles)
    val right = 0
    val up = 90
    val left = 180
    val down = 270

    def move(id: Int, dir: Int): Boolean = {
      val attempt = movementManager.move(id, dir, 1)
      val currentDistance = movementManager.rectangles(id).start.distanceTo(center)
      if (attempt.isDefined) {
        val newDistance = attempt.get.rectangles(id).start.distanceTo(center)
        if (newDistance < currentDistance) {
          movementManager = attempt.get
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    for (i <- rectangles.indices) {
      val r = rectangles(i)
      val cx = r.center.x
      val cy = r.center.y
      val dir1 = if (cx < center.x) Some(right) else if (cx > center.x) Some(left) else None
      val dir2 = if (cy < center.y) Some(up) else if (cx > center.y) Some(down) else None

      if (dir1.isDefined) {
        var done = false
        while (!done) {
          val moved = move(i, dir1.get)
          if (!moved)
            done = true
        }
      }

      if (dir2.isDefined) {
        var done = false
        while (!done) {
          val moved = move(i, dir2.get)
          if (!moved)
            done = true
        }
      }
    }

    movementManager.rectangles
  }

}
