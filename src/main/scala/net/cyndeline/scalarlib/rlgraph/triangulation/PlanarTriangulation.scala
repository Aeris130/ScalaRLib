package net.cyndeline.scalarlib.rlgraph.triangulation

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

/**
 * Specifies methods used when triangulating a planar graph.
 */
trait PlanarTriangulation {

  /**
   * Computes the edges needed to be added to a graph in order to triangulate it.
   * @param graph A planar graph to triangulate. Can be disconnected.
   */
  def triangulate[VType: TypeTag : ClassTag](graph: Graph[VType, UnDiEdge]): Option[TriangulationEdges[VType]]

}
