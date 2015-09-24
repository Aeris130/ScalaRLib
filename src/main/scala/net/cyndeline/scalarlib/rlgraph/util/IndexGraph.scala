package net.cyndeline.scalarlib.rlgraph.util

import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag

/**
 * Wraps every vertex in an undirected graph into a vertex with an integer index 0->(n-1), where n is the graphs node
 * size. This allows algorithms to perform array lookups using vertices.
 */
object IndexGraph {
  def undirected[VType : TypeTag](graph: Graph[VType, UnDiEdge]): Graph[IndexVertex[VType], UnDiEdge] = {
    var result = Graph[IndexVertex[VType], UnDiEdge]()
    val vertices = new mutable.HashMap[VType, IndexVertex[VType]]()
    var i = 0

    for (n <- graph.nodes) {
      val indexV: IndexVertex[VType] = IndexVertex(n, i)
      result += indexV
      vertices += ((n, indexV))
      i += 1
    }

    for (e <- graph.edges) {
      val a = vertices(e._1)
      val b = vertices(e._2)
      result += a~b
    }

    result
  }

  def directed[VType : TypeTag](graph: Graph[VType, DiEdge]): Graph[IndexVertex[VType], DiEdge] = {
    var result = Graph[IndexVertex[VType], DiEdge]()
    val vertices = new mutable.HashMap[VType, IndexVertex[VType]]()
    var i = 0

    for (n <- graph.nodes) {
      val indexV: IndexVertex[VType] = IndexVertex(n, i)
      result += indexV
      vertices += ((n, indexV))
      i += 1
    }

    for (e <- graph.edges) {
      val a = vertices(e._1)
      val b = vertices(e._2)
      result += a~>b
    }

    result
  }

}

case class IndexVertex[VType](valueOfIndex: VType, index: Int) {
  override val toString: String = "[" + valueOfIndex + " with index " + index + "]"
}
