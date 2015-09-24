package net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help

import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Increases the size of every rectangles size by a constant if the rectangle falls short of a minimum side size.
 */
trait MinimumSizeIncrease {
  def increaseToMinimum[V, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E], min: Int): RectangularLayout[V, E]
  def increaseToMinimumWithExceptions[V, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E], exceptions: Set[V], min: Int): RectangularLayout[V, E]
}
