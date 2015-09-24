package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.PartitionedArea
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * Creates partitioned grid structures.
 */
trait GridPartitionFactory[E] {

  /**
   * @param start The start coordinate of the grid.
   * @param stop The stop coordinate of the grid. Both x and y must be higher than the start coordinate.
   * @param roomAmount The number of room areas to be added.
   * @return A grid partition.
   */
  def createGrid(start: Point, stop: Point, roomAmount: Int): PartitionedArea[E]
}
