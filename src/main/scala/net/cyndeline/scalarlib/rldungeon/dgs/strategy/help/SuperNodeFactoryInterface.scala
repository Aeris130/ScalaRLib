package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import net.cyndeline.scalarlib.rldungeon.common.Room

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Responsible for merging cycles of a graph into super-nodes representing every vertex in the cycle.
 * Cycles that share vertices are merged into the same node.
 */
trait SuperNodeFactoryInterface {

  /**
    * @param graph A graph without multi-edges.
    * @tparam R Vertex type in the graph.
    * @tparam C Edge type in the graph.
    * @return A graph where every biconnected component has been merged into a collapsed node representing all vertices
    *         inside the component. If the graph contains components with only two nodes, having directed edges going
    *         in both directions, those edges will be replaced with a single undirected edge (allowing traversal in
    *         both directions) in order to keep the graph loop-free.
    */
  def collapseCycles[R <: Room, C[X] <: UnDiEdge[X]](graph: Graph[R, C]): Graph[CollapsedNode, CollapsedEdge]

}
