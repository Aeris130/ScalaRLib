package rldungeon.help

import net.cyndeline.rlgraph.subgraph.isomorphism.ElementEquivalence
import net.cyndeline.scalarlib.rldungeon.common.Room

import scalax.collection.immutable.Graph

/**
 * Used in graph tests with rooms.
 */
class RoomVertex(override val rid: Int) extends Room(rid) {

  override def toString: String = "RoomV:[" + rid + "]"


  override def equals(other: Any): Boolean = other match {
    case r: RoomVertex => r.rid == rid
    case _ => false
  }

  override def hashCode: Int = rid

}

class IdEquivalence extends ElementEquivalence[RoomVertex, CorridorEdge] {
  def compares(e1: RoomVertex, e2: RoomVertex, contextForE1: Graph[RoomVertex, CorridorEdge], contextForE2: Graph[RoomVertex, CorridorEdge]): Boolean = e1.rid == e2.rid

  /**
   * Computes that hash code used when comparing this element, two elements that are equal (in regards to the values
   * being sought after when performing an isomorphic search) should produce the same hash.
   *
   * Example: Assume a vertex element has two fields of type String and Int. If the isomorphic match only depends on
   * the String, then only the String should be used to compute the hash.
   *
   * @return the hash code of the element.
   */
  def elementHash(element: RoomVertex): Int = element.rid
}

/**
 * A room factory that lets you load up pre-created rooms in a list with the order they'll be produced in.
 */
class CustomRoomFactory() {

  def make(id: Int): RoomVertex = {
    new RoomVertex(id)
  }
}
