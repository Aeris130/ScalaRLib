package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.MutableArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.util.GridPartition

/**
 * Creates a grid where each cell represents 30% of the total size for small room amounts, and 10% for higher
 * amounts.
 *
 * @constructor Constructs a new partition factory.
 */
class DefaultPartitionFactory extends GridPartitionFactory[MutableArea] {

  /**
   * @param start The start coordinate of the grid.
   * @param stop The stop coordinate of the grid. Both x and y must be higher than the start coordinate.
   * @param roomAmount The number of room areas to be added.
   * @return A grid partition.
   */
  def createGrid(start: Point, stop: Point, roomAmount: Int): PartitionedArea[MutableArea] = {
    val ratio = if (roomAmount < 20) 0.3 else 0.1
    new GridPartition(start, stop, ratio)
  }
}
