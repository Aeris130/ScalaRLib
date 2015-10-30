package rldungeon.help

import scalax.collection.GraphEdge.{EdgeCopy, NodeProduct, DiEdgeLike}
import scalax.collection.GraphPredef.OuterEdge

/**
 * Directed version of the corridor edge.
 */
class DiCorridorEdge[N](nodes: Product, cid: Int) extends CorridorEdge[N](nodes, cid)
  with DiEdgeLike[N]
  with EdgeCopy[DiCorridorEdge]
  with OuterEdge[N, DiCorridorEdge] {

  override def copy[NN](newNodes: Product) = new DiCorridorEdge[NN](newNodes, cid)

  override def equals(other: Any): Boolean = other match {
    case d: DiCorridorEdge[N] => d.cid == cid && d._1 == _1 && d._2 == _2
    case _ => false
  }

  override def hashCode(): Int = cid

  override def toString: String = "(" + this._1 + "~>" + this._2 + ")[" + cid + "]"

}

object DiCorridorEdge {

  def apply[T](from: T, to: T, id: Int): DiCorridorEdge[T] = new DiCorridorEdge[T](NodeProduct(from, to), id)
  def apply[T](from: T, to: T): DiCorridorEdge[T] = new DiCorridorEdge[T](NodeProduct(from, to), -1)

}
