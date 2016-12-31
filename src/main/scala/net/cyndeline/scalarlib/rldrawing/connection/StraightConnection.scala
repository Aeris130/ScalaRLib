package net.cyndeline.scalarlib.rldrawing.connection

import net.cyndeline.rlcommon.math.Interval
import net.cyndeline.rlcommon.math.geom.spatialIndex.common.ElementProperty
import net.cyndeline.rlcommon.math.geom.spatialIndex.intervalTree.IntervalTree
import net.cyndeline.rlcommon.math.geom.{Point, Rectangle}
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.scalarlib.rldrawing.common._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Adds rectangular connections between rooms where the two rooms being connected are facing each other,
  * allowing for a straight connection between them.
  *
  * @param connectionDistance The number of coordinates in each direction from a rectangle to look for neighbors in.
  * @param minConnection Smallest number of shared coordinates between two rectangles that is required to connect them.
  * @param maxConnection The largest number of shared coordinates that should be used in a connection.
  */
class StraightConnection(val connectionDistance: Int,
                         val minConnection: Int,
                         val maxConnection: Int) {
  require(minConnection >= 3, "Minimum connection length must be 3 or greater.")
  require(minConnection <= maxConnection, "Minimum connection must be <= max connection.")
  require(connectionDistance > 0, "Connection distance must be greater than 0.")

  /**
    * @param connectionRooms Rooms to add connections between.
    * @param allRooms Every room to take into consideration when connecting. Must contain every room that
    *                 has connections added to it.
    * @return Every new room added. Will share edges with the rooms it connects to.
    */
  def addConnections(connectionRooms: Vector[Rectangle], allRooms: Vector[Rectangle]): Vector[Rectangle] = {
    if (connectionRooms.isEmpty)
      return Vector()

    require(allRooms.intersect(connectionRooms) == connectionRooms, "Every room to connect must also be present in the complete room collection.")
    var tree: KDTree[Rectangle, Rectangle] = KDTree.rectangleTree(allRooms)
    val balanceAt = allRooms.length / 2
    var currentIteration = 0

    val result = new ArrayBuffer[Rectangle]()
    val resultSet = new mutable.HashSet[Rectangle]()
    val directions = Seq(North, West, South, East)
    val connectionsIt = connectionRooms.iterator

    while (connectionsIt.nonEmpty) {
      val room = connectionsIt.next()
      val dIt = directions.iterator

      while (dIt.hasNext) {
        val d = dIt.next()
        val newAreas = connectSide(room, tree, d, resultSet)
        val areaIt = newAreas.iterator

        while (areaIt.hasNext) {
          val area = areaIt.next()
          tree = tree.insert(area)
          currentIteration += 1
          if (currentIteration > balanceAt) {
            currentIteration = 0
            tree = tree.balance
          }
        }

        result ++= newAreas
        resultSet ++= newAreas
      }
    }

    result.toVector
  }

  /**
    * Connects a rectangle r to any potential neighbors on a specified side.
    * @param r Rectangle to connect.
    * @param tree Tree with all rectangles in it, including rectangles added by the algorithm.
    * @param side Side to connect to neighbors on.
    * @param prevConnections Set containing all rectangles that were previously added as connections.
    * @return All connections made.
    */
  private def connectSide(r: Rectangle,
                          tree: KDTree[Rectangle, Rectangle],
                          side: Direction,
                          prevConnections: mutable.HashSet[Rectangle]): Vector[Rectangle] = {
    val result = new ArrayBuffer[Rectangle]()
    val neighbors = {
      val sorted = findNeighbors(r, tree, side).sortBy(n => DirectionProperty.coordinate(n, side))

      /* The sort places neighbors with the smallest coordinate at the head of the list. This will only be the
       * position closest to the rectangle if we're looking at neighbors to the north or east, otherwise we have
       * to reverse the list when looking at south and west.
       */
      side match {
        case North | East => sorted
        case South | West => sorted.reverse
      }
    }

    val rCover = DirectionProperty.interval(r, side)

    /* This interval tree keeps track of which intervals on the axis parallel with the current direction that
     * have already been covered by a neighbor and connected to R.
     */
    var intervals = IntervalTree.empty[Int](ElementProperty.intProperty)

    def add(from: Int, to: Int): Unit = {
      intervals = intervals.insert(from, to)
    }

    val ns = neighbors.iterator
    while (ns.hasNext) {
      val neighbor = ns.next()

      if (neighbor != r) {
        val cover = neighborCover(rCover, neighbor, side)

        // No need to connect areas that already share a border, but they must still add their cover to the interval tree
        if (neighbor.intersection(r).nonEmpty) {

          /* Since this neighbor shares a border with R, its covered interval can safely be added without subtracting
           * covers that (cannot) come before it.
           */
          add(cover._1, cover._2)

        } else {
          val overlaps = intervals.search(cover._1, cover._2).toVector.sortBy(_._1) // No intervals with overlapping start coordinates, so sort is unique

          // Take the cover, and subtract all overlapping intervals from it
          val visibleFromR = Interval(cover._1, cover._2).diff(overlaps.map(o => Interval(o._1, o._2)))
            .filter(i => {

              /* The interval gets updated with all intervals, even the ones that are too small to be connected. */
              add(i.from, i.to)

              i.to - i.from >= minConnection - 1
            })

          /* Connect the visible portions of the neighbor to R, and register the intervals in the tree. */
          val visibleIt = visibleFromR.iterator
          while (visibleIt.hasNext) {
            val i = visibleIt.next()

            if (!prevConnections.contains(neighbor))
              result += join(r, neighbor, side, i.from, i.to)
          }
        }
      }
    }
    result.toVector
  }

  /** Computes the cover for a neighbor N, and limits it to the span of R. */
  private def neighborCover(rCover: (Int, Int), n: Rectangle, s: Direction): (Int, Int) = {
    val nc = DirectionProperty.interval(n, s) // Doesn't matter if we use the search side or its opposite here

    /* If the neighbors cover exceeds the rectangles, limit it to the rectangle. */
    (if (rCover._1 > nc._1) rCover._1 else nc._1, if (rCover._2 < nc._2) rCover._2 else nc._2)
  }

  private def findNeighbors(r: Rectangle, tree: KDTree[Rectangle, Rectangle], side: Direction): Vector[Rectangle] = {
    val area = searchArea(r, side)
    tree.rangeSearch(area)
  }

  /**
    * Constructs a rectangular area sharing the border of a rectangle R at a specified side.
    *
    * The area will have width or height N, where N is the connection distance. The other side will have the same length
    * as the side of R it is sharing.
    *
    * @param r Rectangle to share border with.
    * @param side Side of r to construct are on.
    */
  private def searchArea(r: Rectangle, side: Direction): Rectangle = side match {
    case North => Rectangle(Point(r.start.x, r.stop.y), r.width, connectionDistance)
    case West => Rectangle(Point(r.start.x - connectionDistance + 1, r.start.y), connectionDistance, r.height)
    case South => Rectangle(Point(r.start.x, r.start.y  - connectionDistance + 1), r.width, connectionDistance)
    case East => Rectangle(Point(r.stop.x, r.start.y), connectionDistance, r.height)
  }

  /** Joins a rectangles R with its neighbor N with a rectangle respecting the min/max connection sizes.
    *
    * @param side The side of R that points towards N.
    */
  private def join(r: Rectangle, n: Rectangle, side: Direction, from: Int, to: Int): Rectangle = {
    val rStart = DirectionProperty.coordinate(r, side)
    val nStart = DirectionProperty.coordinate(n, side.opposite)
    val low = Math.min(rStart, nStart)
    val high = if (low == rStart) nStart else rStart
    val opposite = shrinkCover(from, to) // y if low/high is x and vice versa
    val connectionStart = side match {
      case North | South => Point(opposite._1, low)
      case West | East => Point(low, opposite._1)
    }
    val connectionStop = side match {
      case North | South => Point(opposite._2, high)
      case West | East => Point(high, opposite._2)
    }
    Rectangle(connectionStart, connectionStop)
  }

  /** Reduces an interval to the max connection size, centered around the middle of the original interval. */
  private def shrinkCover(from: Int, to: Int): (Int, Int) = {
    val distance = to - from
    assert(from <= to)
    if (distance < maxConnection) {
      (from, to)
    } else {
      val center: Int = Math.floor(distance / 2d).toInt
      val halfMax = Math.floor(maxConnection / 2d).toInt
      val start = center - halfMax
      val stop = if (maxConnection % 2 == 0) center + halfMax + 1 else center + halfMax
      (start, stop)
    }
  }


}
