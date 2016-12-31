package net.cyndeline.scalarlib.rldrawing.dungeon.separation

import net.cyndeline.rlcommon.math.geom.{Dimensions, Rectangle}
import net.cyndeline.rlcommon.util.Matrix2DOp

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Tobias Edin on 2016-12-29.
  *
  * @param size Width and height of the grid.
  */
class PlacementGrid private (val size: Int, largest: Int, grid: Array[Array[Int]]) {
  private val occupied = 0
  private val areas = new ArrayBuffer[Rectangle]

  /**
    * @param x A coordinate along the x axis on the grid.
    * @param y A coordinate along the y axis on the grid.
    * @return The width and height of the largest possible square available, where the starting corner of the
    *         square is this (x,y) coordinate. 0 if the coordinate is occupied by a rectangle (except at the border).
    */
  def spaceAt(x: Int, y: Int): Int = grid(x)(y)

  def isOccupied(x: Int, y: Int): Boolean = x < 0 || y < 0 || x >= size || y >= size || spaceAt(x, y) < 1

  /**
    * @param d Dimensions of rectangle to place.
    * @param x X coordinate to place rectangle start at.
    * @param y Y coordinate to place rectangle start at.
    * @return True if a rectangle with dimensions d can start at (x,y) without overlapping other rectangles (may share
    *         borders), otherwise false.
    */
  def canBePlaced(d: Dimensions, x: Int, y: Int): Boolean = if (!isOccupied(x, y)) {

    /* This grid only works in squares. If the cell (x,y) is the start of a square that fits R along one axis,
     * but not the other, the square length in (x,y) is used to compute the coordinate for the next cell needed
     * to fit the remaining part of R. This continues until R is considered to fit, or until an occupied cell if found.
     */

    val squareSize = spaceAt(x, y)
    if (squareSize >= d.width && squareSize >= d.height) {
      true

    } else if (squareSize >= d.width || squareSize >= d.height) {

      def expand(x: Int, y: Int, remainsOfR: Int, xAxis: Boolean, otherAxis: Int): Boolean = if (!isOccupied(x, y)) {
        val square = spaceAt(x, y)
        val remains = Math.max(remainsOfR - square, 0)

        /* Even though this method recursion is only started if the initial square fits the area on one axis, it's
         * possible that it ends up on a cell whose square is too narrow further up. If so, abort.
         */
        if (square < otherAxis)
          return false

        if (remains == 0) {
          true
        } else {
          if (xAxis)
            expand(x + square, y, remains, xAxis, otherAxis)
          else
            expand(x, y + square, remains, xAxis, otherAxis)
        }

      } else {
        false
      }

      /* The rectangle fits along one axis, but the other needs expanding. */
      if (squareSize >= d.height)
        expand(x, y, d.width, true, d.height) // R's height is within the square, check the next cell for more width
      else
        expand(x, y, d.height, false, d.width)

    } else {
      false
    }

  } else {
    false
  }

  def place(r: Rectangle): Unit = {
    require(r.start.x >= 0 && r.start.y >= 0 && r.stop.x < size && r.stop.y < size, s"Attempted to add rectangle $r outside grid bounds.")
    checkOverlap(r)

    for (i <- 1 to r.width - 2; j <- 1 to r.height - 2)
      grid(r.start.x + i)(r.start.y + j) = occupied

    /* When placing a rectangle R, every coordinate below its upper corner must be updated if it currently marks
     * coordinates above start(x,y) of the rectangle as part of its largest square. We start with the tiles closest
     * to stop(x,y) of R and work backwards towards (0,0) along both axises. The update stops once it goes further
     * than the largest possible area side length since no coordinate past that can have R within its largest possible
     * area.
     */
    val updateStart = r.stop
    val largestX = largest + r.width
    val largestY = largest + r.height
    var i = 1 // Start at one since borders may overlap
    while (i < largestX && updateStart.x - i >= 0) {
      var j = 1
      while (j < largestY && updateStart.y - j >= 0) {
        val x = updateStart.x - i
        val y = updateStart.y - j
        val largestSquare = spaceAt(x, y)
        if (largestSquare > 0 && x < updateStart.x && y < updateStart.y) {

          /* If the side length causes a [side * side] square to overlap R, then it needs to be decreased such that
           * its upper corner lies on R.start.
           */
          if (x + largestSquare > r.start.x || y + largestSquare > r.start.y) {

            /* Note: Cells containing area borders will always be set to 1, even if multiple borders are present.
             * However, since the placement algorithm only takes dimensions of size 3+, this should in practice
             * be the same thing as marking the cell occupied.
             *
             * If the cell currently has a smaller value stored, use that instead since that means another area
             * already blocks it off. Values can only decrease, not increase.
             */
            grid(x)(y) = Math.min(grid(x)(y), Math.max(r.start.x - x, r.start.y - y) + 1)
          }

        }
        j += 1
      }
      i += 1
    }

  }

  private def checkOverlap(r: Rectangle): Unit = {
    val start = r.start
    val stop = r.stop
    for (i <- start.x to stop.x; j <- start.y to stop.y)
      if (this.spaceAt(i, j) == occupied)
        throw new Error("Attempted to place area that overlaps inside of another area.")
  }

}

object PlacementGrid {

  /**
    * @param size Size of the grid (both width and height).
    * @param largestAreaSide The largest number of coordinates an area may have along either axis.
    */
  def apply(size: Int, largestAreaSide: Int): PlacementGrid = {
    require(size > 0, "Cannot create empty grid")
    val grid = Array.fill(size, size)(-1)

    /* Set the largest available square for every tile to the square going from that tile to the upper right corner,
     * or to the largest area side, since no area will exceed that size anyway.
     */
    def setSquare(current: Int, x: Int, y: Int): Int = {
      Math.min(size - Math.max(x, y), largestAreaSide)
    }
    Matrix2DOp.modify(grid, setSquare _)

    new PlacementGrid(size, largestAreaSide, grid)
  }

}
