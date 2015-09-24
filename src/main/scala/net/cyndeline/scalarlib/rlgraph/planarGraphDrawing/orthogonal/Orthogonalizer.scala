package net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.orthogonal

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.orthogonal.drawing.OrthogonalLayout

/**
 * Computes an orthogonal drawing of a graph.
 */
trait Orthogonalizer[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * Computes an orthogonal drawing.
   * @param graph A connected planar graph with vertex degree 4 or lower.
   * @return an orthogonal drawing of the graph.
   */
  def orthogonalize(graph: Graph[VType, EType]): OrthogonalLayout[VType, EType]
}
