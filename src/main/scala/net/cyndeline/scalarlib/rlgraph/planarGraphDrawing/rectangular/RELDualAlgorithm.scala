package net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular

import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rlgraph.regularEdgeLabeling.EdgeLabeling
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.dataStructure.RVertex

/**
 * An algorithm that produces rectangular duals based on a single regular edge labeling, and any labelings that
 * result from flipping its edges. Note that the user is responsible for providing a correct edge labeling, as the
 * result will otherwise be unspecified.
 *
 * The REL algorithm uses labelings that wrap their vertices in an RVertex object. This is needed since the labeling
 * must be able to represent the four outer vertices as well as the edge splits, both of which are not present in the
 * input graph.
 */
trait RELDualAlgorithm[VType, EType[X] <: UnDiEdge[X]] {

  /**
   * @param rel A valid regular edge labeling.
   * @return A rectangular layout of the REL.
   */
  def computeLayout(rel: EdgeLabeling[RVertex[VType]]): RectangularLayout[VType, EType]

}
