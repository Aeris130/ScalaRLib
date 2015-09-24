package net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.evolution.metrics

import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.evolution.FitnessScore
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import net.cyndeline.scalarlib.rlgraph.cartogram.rectangular.common.MapArea
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.dataStructure.Rectangle
import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rldrawing.util.Geom

/**
 * Computes the average of squared cartographic errors for a layout. Since the score doesn't increase for more
 * fit layouts, the result N is returned as 1/N, with the minimum value of N equal to 0.0001. Since the algorithm
 * doesn't regularly produce layouts with the cartographic error this low, it won't affect the layout quality.
 *
 * @param weight Value to multiply the final result by.
 */
class CartographicError(weight: Double) extends FitnessScore {

  def computeScore[V <: MapArea, E[X] <: UnDiEdge[X]](layout: RectangularLayout[V, E]): Double = {
    val rectToVertex = layout.rectangles.map(_.swap)
    val rectanglesWithSpecifiedArea = layout.allAreas.filter(_.isVertex)
    val average: Double = rectanglesWithSpecifiedArea
      .map(a => error(a, rectToVertex(a).targetArea))
      .map(error => error * error)
      .sum

    (average / rectanglesWithSpecifiedArea.size) * weight
  }

  private def error(r: Rectangle, target: Int): Double = {
    require(r.isVertex)
    val area = Geom.area(r.startX, r.stopX, r.startY, r.stopY)
    Math.abs(area - target).toDouble / target
  }
}
