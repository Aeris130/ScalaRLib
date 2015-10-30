package rldungeon.help

import net.cyndeline.rlgraph.subgraph.isomorphism.{VertexCompare, ElementEquivalence}
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

class IdEquivalence extends VertexCompare[RoomVertex, Int] {
  override def compareNode(v1: RoomVertex, v2: Int): Boolean = v1.rid == v2
}

/**
 * A room factory that lets you load up pre-created rooms in a list with the order they'll be produced in.
 */
class CustomRoomFactory() {

  def make(id: Int): RoomVertex = {
    new RoomVertex(id)
  }
}
