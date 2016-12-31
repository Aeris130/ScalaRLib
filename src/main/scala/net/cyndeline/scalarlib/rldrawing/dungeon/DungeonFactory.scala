package net.cyndeline.scalarlib.rldrawing.dungeon

import net.cyndeline.rlcommon.math.geom.Dimensions
import net.cyndeline.scalarlib.rldrawing.{ForceGridLayout, MapLayout}
import net.cyndeline.scalarlib.rldrawing.common.RRoom
import net.cyndeline.scalarlib.rldrawing.connection.StraightConnection
import net.cyndeline.scalarlib.rldrawing.dungeon.separation.RectanglePlacement

/**
  * Places a number of rectangular areas close to each other, then connects nearby areas using hub spaces.
  */
class DungeonFactory(seed: Int, minConnectionSize: Int, maxConnectionSize: Int) {
  require(minConnectionSize <= maxConnectionSize, "Minimum connection size must be <= max connection size.")

  /**
    * @param areas A list of all areas (specified as width/height) that should be placed on the map.
    * @return A connected map containing all areas.
    */
  def generateLayout(areas: Vector[Dimensions]): MapLayout[RRoom] = {
    require(!areas.exists(d => d.height < 1 || d.width < 1), "Every room dimension must have width and height greater than 0.")
    val placement = new RectanglePlacement()
    val separated = placement.computePlacement(areas).getOrElse(throw new Error("Failed to place all areas."))

    /* Use the average room length * 2 when determining how far to look for neighbors to connect. */
    val averageLength = Math.ceil(areas.map(d => Math.max(d.height, d.width)).sum / areas.length.toDouble).toInt
    val straitConnectionAlgorithm = new StraightConnection(averageLength, minConnectionSize, maxConnectionSize)
    val connections = straitConnectionAlgorithm.addConnections(separated, separated)

    ForceGridLayout(separated, connections)
  }

}
