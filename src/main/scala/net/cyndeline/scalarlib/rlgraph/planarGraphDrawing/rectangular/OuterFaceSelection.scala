package net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rlgraph.face.Face

/**
 * This implementation is supplied by the user, and is used to select the outer face of a graph for the final
 * rectangular drawing.
 */
trait OuterFaceSelection[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @param faces The biconnected faces of the graph, before it has become triconnected.
   * @return The face that should be used as an outer face of the drawing.
   */
  def select(faces: Vector[Face[VType]], graph: Graph[VType, EType]): Face[VType]

}
