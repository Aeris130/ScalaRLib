package rldrawing.help

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.CorridorProperties
import scalax.collection.GraphEdge.{NodeProduct, EdgeCopy, UnDiEdge}
import scalax.collection.GraphPredef.OuterEdge

/**
 * An edge with a width.
 */
class ConstraintEdge[N](nodes: Product, width: Int, minLength: Int, maxDeviation: Int) extends UnDiEdge[N](nodes)
  with EdgeCopy[ConstraintEdge]
  with OuterEdge[N,ConstraintEdge]
  with CorridorProperties {

  def this(nodes: Product, width: Int) = this(nodes, width, 1, 0)

  override def elementWidth: Int = width

  override def minimumLength: Int = minLength

  override def boundaryDeviation: Int = maxDeviation

  override def copy[NN](newNodes: Product) = new ConstraintEdge[NN](newNodes, width, minLength, maxDeviation)
}

object ConstraintEdge {
  def apply[V](from: V, to: V, width: Int) = new ConstraintEdge[V](NodeProduct(from, to), width)
  def apply[V](from: V, to: V, width: Int, minLength: Int) = new ConstraintEdge[V](NodeProduct(from, to), width, minLength, 0)
  def apply[V](from: V, to: V, width: Int, minLength: Int, maxDeviation: Int) = new ConstraintEdge[V](NodeProduct(from, to), width, minLength, maxDeviation)
}