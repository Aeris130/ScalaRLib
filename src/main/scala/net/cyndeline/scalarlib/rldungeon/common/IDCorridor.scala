package net.cyndeline.scalarlib.rldungeon.common

import scalax.collection.GraphEdge.{EdgeCopy, NodeProduct, UnDiEdge}
import scalax.collection.GraphPredef.OuterEdge

/**
 * Basic edge used to represent a user-specified corridor by using the same cid.
 */
class IDCorridor[N](nodes: Product, id: Int) extends UnDiEdge[N](nodes)
  with EdgeCopy[IDCorridor]
  with OuterEdge[N, IDCorridor]
  with Corridor {

  override def cid: Int = id

  override def copy[NN](newNodes: Product) = new IDCorridor[NN](newNodes, id)

}

object IDCorridor {
  def apply[V](from: V, to: V, id: Int): IDCorridor[V] = new IDCorridor(NodeProduct(from, to), id)
  def unapply[V](e: IDCorridor[V]): Option[(V, V, Int)] = if (e == null) None else Some((e._1, e._2, e.cid))
}
