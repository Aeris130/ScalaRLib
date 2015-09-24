package rldungeon.help

import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.ElementEquivalence
import scalax.collection.immutable.Graph

/**
 * Matches any room vertices.
 */
class RoomVertexEquivalenceAny extends ElementEquivalence[RoomVertex, CorridorEdge] {

  def compares(e1: RoomVertex, e2: RoomVertex, contextForE1: Graph[RoomVertex, CorridorEdge], contextForE2: Graph[RoomVertex, CorridorEdge]): Boolean = true

  def elementHash(element: RoomVertex): Int = 1
}
