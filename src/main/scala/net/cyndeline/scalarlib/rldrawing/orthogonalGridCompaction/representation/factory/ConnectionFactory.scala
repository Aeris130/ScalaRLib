package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation._
import net.cyndeline.scalarlib.rldrawing.util.Direction._

/**
 * Creates connections in both directions between two areas and a corridor.
 *
 * @constructor Constructs a new connection factory.
 */
class ConnectionFactory {

  /**
   * Updates connection info in two areas and a corridor with eachothers connection info.
   *
   * @param from Initial room that the corridor connects from.
   * @param corridor The corridor.
   * @param to The second room that the corridor connects to.
   * @param direction The side of the initial room that the corridor connects on.
   * @param deviation The number of tiles that the corridor is allowed to slide across the boundary of either room.
   */
  def connect(from: MutableArea, corridor: CorridorArea, to: MutableArea, direction: Direction, deviation: Int) {
    val area1C = makeConnection(from, corridor, direction, deviation)
    from.connect(direction, area1C)
    corridor.connect(direction.opposite, area1C)

    val area2C = makeConnection(to, corridor, direction.opposite, deviation)
    to.connect(direction.opposite, area2C)
    corridor.connect(direction, area2C)

  }

  private def makeConnection(room: MutableArea, corridor: CorridorArea, connectionDir: Direction, deviation: Int): Connection = {
    room match {
      case b: CorridorBend => new Connection(b, corridor, connectionDir, deviation)
      case r: RoomArea => new Connection(r, corridor, connectionDir, deviation)
      case _ => throw new Error("Found a mutable area that weren't a room or bend, instead was " + room.getClass)
    }
  }

}
