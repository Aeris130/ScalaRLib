package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import scalax.collection.GraphEdge.{DiEdge, NodeProduct, DiEdgeLike, EdgeCopy}
import scalax.collection.GraphPredef.OuterEdge

/**
 * Directed version of collapsed edges.
 */
class DiCollapsedEdge[N](nodes: Product, originalFrom: Option[Int], originalTo: Option[Int], isDummy: Boolean = false)
  extends CollapsedEdge[N](nodes, originalFrom, originalTo, isDummy)
  with EdgeCopy[DiCollapsedEdge]
  with OuterEdge[N, DiCollapsedEdge]
  with DiEdgeLike[N] {

  override def copy[NN](newNodes: Product) = new DiCollapsedEdge[NN](newNodes, originalFrom, originalTo, isDummy)

  override def hashCode(): Int = this._1.## ^ this._2.## ^ originalFrom.## ^ originalTo.## ^ isDummy.## << 32

  override def equals(other: Any): Boolean = other match {
    case ce: DiCollapsedEdge[N] => ((ce._1 == this._1 && ce._2 == this._2) || (ce._1 == this._2 && ce._2 == this._1)) && ce.originalFrom == originalFrom && ce.originalTo == originalTo && ce.isDummy == isDummy
    case _ => false
  }
}

final class DiCollapsedEdgeAssoc[A <: CollapsedNode](val e: DiEdge[A]) {
  def emptyEdge(): DiCollapsedEdge[A] = new DiCollapsedEdge(NodeProduct(e._1, e._2), None, None)
  def setOriginalTargets(originalFrom: Option[Int], originalTo: Option[Int]): DiCollapsedEdge[A] =
    new DiCollapsedEdge(NodeProduct(e._1, e._2), originalFrom, originalTo)
}

object DiCollapsedEdge {

  def apply(from: CollapsedNode, to: CollapsedNode): DiCollapsedEdge[CollapsedNode] = new DiCollapsedEdge[CollapsedNode](NodeProduct(from, to), None, None)
  def apply(from: CollapsedNode, to: CollapsedNode, originalFrom: Option[Int], originalTo: Option[Int]): DiCollapsedEdge[CollapsedNode]=
    new DiCollapsedEdge(NodeProduct(from, to), originalFrom, originalTo)

}
