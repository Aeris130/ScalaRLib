package rldungeon.help

import scalax.collection.GraphEdge.{ExtendedKey, NodeProduct, EdgeCopy, UnDiEdge}
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rlgraph.util.{EdgeFactory, EdgeCopyFactory}
import net.cyndeline.scalarlib.rldungeon.common.{Corridor, Room}


class CorridorEdge[N](nodes: Product, val cid: Int) extends UnDiEdge[N](nodes)
  with EdgeCopy[CorridorEdge]
  with ExtendedKey[N]
  with OuterEdge[N, CorridorEdge]
  with Corridor {

  override def copy[NN](newNodes: Product) = new CorridorEdge[NN](newNodes, cid)

  def keyAttributes: Seq[Any] = Seq(cid)

  override def toString: String = "(" + this._1 + "~" + this._2 + ")<" + cid + ">"
}

final class CorridorCopyFactory[V <: Room] extends EdgeCopyFactory[V, CorridorEdge] {
  def copyEdge(edge: CorridorEdge[V], a: V, b: V): CorridorEdge[V] with OuterEdge[V, CorridorEdge] = CorridorEdge(a, b)
}

final class CorridorFactory[V <: Room] extends EdgeFactory[V, CorridorEdge] {
  def produce(from: V, to: V): CorridorEdge[V] with OuterEdge[V, CorridorEdge] = CorridorEdge(from, to)
}

final class CorridorEdgeAssoc[V](val e: UnDiEdge[V]) {
  def cid(id: Int): CorridorEdge[V] = CorridorEdge(e._1, e._2, id)
  def empty = CorridorEdge(e._1, e._2)
}

object CorridorEdge {
  def apply[V](from: V, to: V) = new CorridorEdge[V](NodeProduct(from, to), -1)
  def apply[V](from: V, to: V, id: Int) = new CorridorEdge[V](NodeProduct(from, to), id)
}
