package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import net.cyndeline.scalarlib.rldungeon.common.Room

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Responsible for merging cycles of a graph into super-nodes representing every vertex in the cycle.
 * Cycles that share vertices are merged into the same node.
 */
trait SuperNodeFactoryInterface {

  def collapseCycles[R <: Room, C[X] <: UnDiEdge[X]](graph: Graph[R, C]): Graph[CollapsedNode, CollapsedEdge]

}
