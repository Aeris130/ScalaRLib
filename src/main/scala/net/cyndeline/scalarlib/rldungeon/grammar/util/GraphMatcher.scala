package net.cyndeline.scalarlib.rldungeon.grammar.util

import net.cyndeline.rlgraph.subgraph.isomorphism.{EdgeCompare, VertexCompare}
import net.cyndeline.scalarlib.rldungeon.common.Room

import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Used by isomorphic pattern matches when searching for sub-graphs.
 *
 * @tparam R Vertex type used in level graphs.
 * @tparam E Edge type used in level graphs.
 * @tparam P Vertex type used in pattern graphs.
 * @tparam PE Edge type used in pattern graphs.
 */
class GraphMatcher[R <: Room, E[X] <: EdgeLikeIn[X], P, PE[X] <: EdgeLikeIn[X]] private
  (vertexCompare: Option[VertexCompare[R, P]],
   edgeCompare: Option[EdgeCompare[E, PE]]) {

  def comparesVertices: Boolean = vertexCompare.isDefined
  def comparesEdges: Boolean = edgeCompare.isDefined
  def isEmpty: Boolean = !(comparesVertices ||  comparesEdges)

  def withVertexComparator(vc: VertexCompare[R, P]): GraphMatcher[R, E, P, PE] = new GraphMatcher(Some(vc), edgeCompare)
  def withEdgeComparator(ec: EdgeCompare[E, PE]): GraphMatcher[R, E, P, PE] = new GraphMatcher(vertexCompare, Some(ec))

  def vertexComparator = vertexCompare.getOrElse(throw new Error("No vertex comparator set."))
  def edgeComparator = edgeCompare.getOrElse(throw new Error("No edge comparator set."))
}

/**
 * Factory object for GraphMatchers.
 */
object GraphMatcher {
  def empty[R <: Room, E[X] <: EdgeLikeIn[X], P, PE[X] <: EdgeLikeIn[X]](): GraphMatcher[R, E, P, PE] = new GraphMatcher[R, E, P, PE](None, None)
}
