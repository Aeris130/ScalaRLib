package net.cyndeline.scalarlib.rlgraph.util

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef.EdgeLikeIn
import scala.reflect.runtime.universe.TypeTag

/**
 * The edge set used in scalas graph library doesn't guarantee order based on hash. This object sorts the nodes and
 * edges in a graph based on the hash, and inserts them in order.
 */
object SortedGraph {
  def apply[VType, EType[X] <: EdgeLikeIn[X]: ({type l[M[_]] = TypeTag[M[VType]]})#l](graph: Graph[VType, EType]): Graph[VType, EType] = {
    val edges = graph.edges.map(_.toOuter).toVector.sortBy(_.##)
    val nodes = graph.nodes.toVector.map(n => { val outer: VType = n; outer })
    Graph.from(nodes, edges)
  }
}
