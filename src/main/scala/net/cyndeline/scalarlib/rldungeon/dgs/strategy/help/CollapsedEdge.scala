package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import scalax.collection.GraphEdge.{ExtendedKey, NodeProduct, EdgeCopy, UnDiEdge}
import scalax.collection.GraphPredef.OuterEdge
import scala.Option
import net.cyndeline.scalarlib.rlgraph.util.EdgeCopyFactory
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData
import net.cyndeline.scalarlib.rldungeon.common.Room

/**
 * Connects vertices in a graph that folds biconnected components into super-nodes.
 *
 * Usage: implicit def edge2CollapsedEdgeAssoc[A <: CollapsedNode](e: UnDiEdge[A]) = new CollapsedEdgeAssoc[A](e)
 *
 * @constructor Creates the inner vertex-neutral edge object.
 * @param originalFrom If this edge connects to a super-node in position _1, this value is the vertex in the original
 *                     graph that the vertex inside the super-node originally represented. Example: The edge 1~2.
 *                     Vertex 1 and 2 gets wrapped into collapsed nodes, and the collapsed node that represents 1 later
 *                     is folded into a super-node. This variable would then store the vertex 1.
 * @param originalTo Same as originalFrom, but for _2.
 * @param isDummy True if this edge connects a single node (in either end) representing an articulation point between
 *                two (or more) biconnected components of a graph. This edge doesn't represent an actual edge in the
 *                graph, hence why it's a dummy.
 */
class CollapsedEdge[N](nodes: Product, val originalFrom: Option[Int], val originalTo: Option[Int], val isDummy: Boolean = false) extends UnDiEdge[N](nodes)
  //with ExtendedKey[N]
  with EdgeCopy[CollapsedEdge]
  with OuterEdge[N, CollapsedEdge] {

  override def copy[T](newNodes: Product) = new CollapsedEdge[T](newNodes, originalFrom, originalTo, isDummy)

  override def toString(): String = "\n" + this._1 + "~" + this._2 +
    (if (originalFrom.isDefined) " OF:" + originalFrom.get else "") +
    (if (originalTo.isDefined) " OT:" + originalTo.get else "") +
    (if (isDummy) "-dummy" else "")

  /* Should be ok to only use nodes here, without extended key the original nodes won't be a part of equals,
   * and there won't be two edges between the same node pair anyway.
   */
  override def hashCode(): Int = this._1.## ^ this._2.## ^ originalFrom.## ^ originalTo.## ^ isDummy.## << 32

  override def equals(other: Any): Boolean = other match {
    case ce: CollapsedEdge[N] => ((ce._1 == this._1 && ce._2 == this._2) || (ce._1 == this._2 && ce._2 == this._1)) && ce.originalFrom == originalFrom && ce.originalTo == originalTo && ce.isDummy == isDummy
    case _ => false
  }

}

final class CollapsedEdgeAssoc[A <: CollapsedNode](val e: UnDiEdge[A]) {
  def emptyEdge(): CollapsedEdge[A] = new CollapsedEdge(NodeProduct(e._1, e._2), None, None)
  def setOriginalTargets(originalFrom: Option[Int], originalTo: Option[Int]): CollapsedEdge[A] =
    new CollapsedEdge(NodeProduct(e._1, e._2), originalFrom, originalTo)
}

/**
 * Object used for quick edge creation.
 */
object CollapsedEdge {

  def apply(from: CollapsedNode, to: CollapsedNode): CollapsedEdge[CollapsedNode] = new CollapsedEdge(NodeProduct(from, to), None, None)
  def apply(from: CollapsedNode, to: CollapsedNode, originalFrom: Option[Int], originalTo: Option[Int]): CollapsedEdge[CollapsedNode] =
    new CollapsedEdge(NodeProduct(from, to), originalFrom, originalTo)

  def dummyEdge(from: CollapsedNode, to: CollapsedNode): CollapsedEdge[CollapsedNode] = new CollapsedEdge(NodeProduct(from, to), None, None, true)

  /**
   * Retrieves the represented targets (to/from .1/._2) of a collapsed edge. If the edge targets super-nodes,
   * the original targets are returned instead.
   * @param edge A collapsed edge targeting two collapsed nodes.
   * @tparam T Represented type inside the collapsed nodes.
   * @return The targets represented by the nodes. The first value of the tuple is from/._1, and the second value
   *         is to/._2.
   */
  def targetsOfEdge[T <: Room](edge: CollapsedEdge[CollapsedNode]): (Int, Int) = {
    require(!edge.isDummy, "Cannot retrieve targets of a dummy edge. As a general rule, a dummy edge is connected to two targets: " +
      "a node representing a single vertex (the articulation point) and a node with 2 or more vertices.")
    val firstTarget = if (edge.originalFrom.isDefined) edge.originalFrom.get else edge._1.singleRepresentedVertex
    val secondTarget = if (edge.originalTo.isDefined) edge.originalTo.get else edge._2.singleRepresentedVertex
    (firstTarget, secondTarget)
  }

}
