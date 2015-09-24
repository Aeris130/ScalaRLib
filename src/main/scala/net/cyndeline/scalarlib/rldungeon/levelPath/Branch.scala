package net.cyndeline.scalarlib.rldungeon.levelPath

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef.OuterEdge
import scala.Some
import net.cyndeline.scalarlib.rldungeon.common.Room

/**
 * Connects two nodes in a Tree path. This edge doesn't represent a corridor in a level, it is only used to join
 * one aspect of a level (a room or corridor) with another in the order they're traversed.
 *
 * @param c A common cutpoint shared by two nodes representing biconnected components in the tree.
 */
class Branch[N](nodes: Product, c: Int) extends DiEdge[N](nodes)
  with EdgeCopy[Branch]
  with OuterEdge[N, Branch]
  with LoopFreeEdge[N]
  with ExtendedKey[N] {

  // While it would be more convenient to simply use room id's instead of outer types, the priority here is speed when
  // examining the graph.

  override def copy[NN](newNodes: Product) = new Branch[NN](newNodes, c)
  def keyAttributes: Seq[Any] = Seq(c)

  /** @return A common cutpoint shared by two nodes representing biconnected components in the tree. */
  def connection: Int = c

  override def toString(): String = "(" + this._1 + " " + this.nodesToStringSeparator + " " + this._2 + "<" + connection + ">)"

  override def hashCode(): Int = this._1.## ^ this._2.## ^ c.##


}

/**
 * Users should refer to this object when constructing branches.
 */
object Branch {

  /**
   * @param a A biconnected component.
   * @param b Another biconnected component.
   * @param connection The ID of the common cutpoint the components share.
   * @return An edge between two nodes representing biconnected components sharing a common cutpoint.
   */
  def apply[R <: Room, C[X] <: UnDiEdge[X]](a: TreeNode, b: TreeNode, connection: Int): Branch[TreeNode] = new Branch(NodeProduct(a, b), connection)

  def unapply[R <: Room, C[X] <: UnDiEdge[X]](edge: Branch[TreeNode]): Option[(TreeNode, TreeNode, Int)] = if (edge eq null) {
    None
  } else {
    Some((edge._1, edge._2, edge.connection))
  }

}
