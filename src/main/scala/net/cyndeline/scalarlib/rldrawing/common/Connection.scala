package net.cyndeline.scalarlib.rldrawing.common

/**
  * Signifies that the player may move from one room to another.
  *
  * @param id Unique ID for this connection in the layout it was generated in.
  */
class Connection private (val id: Int, val room1: Int, val room2: Int, directedTo: Option[Int]) {
  require(id >= 0, "Cannot use negative connection id's.")
  require(room1 != room2, "Rooms in connection must be distinct.")
  require(!isDirected || (directedTo.get == room1 || directedTo.get == room2), "Connection directed to room outside the connection.")

  def this(id: Int, r1: Int, r2: Int) = this(id, r1, r2, None)
  def this(id: Int, r1: Int, r2: Int, directedTo: Int) = this(id, r1, r2, Some(directedTo))

  def isDirected: Boolean = directedTo.isDefined
  def direction: Int = directedTo.getOrElse(throw new Error("Cannot retrieve direction from undirected room."))

}

case class ConnectionData(room1: Room, room2: Room, directed: Boolean) {

}
