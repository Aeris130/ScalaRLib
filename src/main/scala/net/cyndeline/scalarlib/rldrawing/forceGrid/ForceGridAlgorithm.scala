package net.cyndeline.scalarlib.rldrawing.forceGrid

import net.cyndeline.rlcommon.math.geom.{Dimensions, Rectangle}
import net.cyndeline.scalarlib.rldrawing.MapLayout
import net.cyndeline.scalarlib.rldrawing.common.RRoom
import net.cyndeline.scalarlib.rldrawing.connection.StraightConnection
import net.cyndeline.scalarlib.rldrawing.forceGrid.separation.RectangleSeparation

/**
  * Created by Tobias Edin on 2016-10-04.
  */
class ForceGridAlgorithm(seed: Int, minConnectionSize: Int, maxConnectionSize: Int) {
  require(minConnectionSize <= maxConnectionSize, "Minimum connection size must be <= max connection size.")

  def generateLayout(rooms: Vector[Dimensions]): MapLayout[RRoom] = {
    require(!rooms.exists(d => d.height < 1 || d.width < 1), "Every room dimension must have width and height greater than 0.")
    val separation = new RectangleSeparation()
    val separated: Vector[Rectangle] = separation.separate(rooms, seed)

    /* Use the average room length * 2 when determining how far to look for neighbors to connect. */
    val averageLength = Math.ceil(rooms.map(d => Math.max(d.height, d.width)).sum / rooms.length.toDouble).toInt
    val straitConnectionAlgorithm = new StraightConnection(averageLength, minConnectionSize, maxConnectionSize)
    val connections = straitConnectionAlgorithm.addConnections(separated, separated)


    /* Algorithm:
     *
     * 1: Compute the initial separated rectangles.
     * 2: Take all disjoint rectangles and pull them towards the center.
     * 3: Construct an initial layout.
     * 4: Take all disjoint groups of vertices (other than the largest one) and pull them towards a random vertex.
     * 5: Connect with hubs
     */

    ForceGridLayout(separated, connections)
  }

}
