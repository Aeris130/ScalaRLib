package net.cyndeline.scalarlib.rldrawing

import net.cyndeline.rlcommon.math.geom.Shape
import net.cyndeline.scalarlib.rldrawing.common.{Connection, ConnectionData, Room}

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
  * Represents map layouts as a series of polygonal areas, along with connectivity. Adjacent area shapes may share
  * edges.
  *
  * The map layout facilitates a fundamental step in map algorithms: To modify a level by adding/removing rooms and
  * corridors. To ensure constraints set by the generating algorithm, only rooms/corridors that were originally
  * in the computed output may be worked with. An edge may only be removed if it is in the current connection
  * graph, but it may also only be added if the initial layout contained it. By only working with the initial
  * room/corridor set, connectivity and planarity is preserved.
  *
  * Since a fundamental aspect of map generation is the connection of disjoint areas, the layout lets the user query
  * if a specific area was added to facilitate such a connection using the isHub method. A hub is an area added
  * to bridge two areas, though it may share borders with more than that.
  *
  * A note on connections and openings: An opening is one or more coordinates that, when allowing for traversal, allows
  * the player to move from one room to another. These openings only occurs where the borders of two rooms overlap.
  * A connection on the other hand simply signifies that two rooms may be traversed between, possible by using other
  * means than crossing their borders. Two rooms may have multiple openings between them, but only a single connection.
  *
  * Connections are also the only element where the user may add additional instances that weren't a part of the
  * initial layout.
  */
abstract class MapLayout[+S <: Shape[S] with Room]() {

  /** @return How many coordinates the drawing occupies along the x axis (starting at 0). */
  def width: Int

  /** @return How many coordinates the drawing occupies along the y axis (starting at 0). */
  def height: Int

  /** @return Every room shape in the layout, including the ones that are removed. */
  def rooms: Vector[S]

  /**
    * @return Room connections represented as a graph, where an edge between vertices m and n represents a
    *         connection between rooms n and m. A vertex n represents the room stored at index n in the room index.
    */
  def connectionGraph: Graph[Int, UnDiEdge]

  /**
    * Creates a connection not already in the layout.
    * @param data Connection data to add to the layout.
    * @return A copy of the layout with the connection created.
    */
  def createConnection(data: ConnectionData): MapLayout[S]

  /**
    * Updates connection data, including which rooms that are connected.
    * @param connection Old connection to update.
    * @param update New data to apply to the connection.
    * @return A copy of the layout with the connection updated.
    */
  def modifyConnection(connection: Connection, update: ConnectionData): MapLayout[S]

  /**
    * Adds an initial connection to the layout based on its id.
    * @param connection Connection to add back into the layout.
    * @return A copy of the layout with the connection added, if it didn't already exist.
    */
  def addConnection(connection: Connection): MapLayout[S]

  /**
    * Removes an initial connection from the layout based on its id.
    * @param connection Connection to remove from the layout.
    * @return A copy of the layout with the connection removed, if it existed in the layout.
    */
  def removeConnection(connection: Connection): MapLayout[S]

  /**
    * @param room A room in the layout.
    * @return Every connection for the room, including the removed ones.
    */
  def connectionsFor(room: Room): Vector[Connection]

  /**
    * Adds an initial room back into the layout based on its id.
    * @param room Room to add back into the layout.
    * @return A copy of the layout with the room added, if it didn't already exist.
    */
  def addRoom(room: Room): MapLayout[S]

  /**
    * Removes an initial room from the layout based on its id.
    * @param room Room to remove from the layout.
    * @return A copy of the layout with the room removed, if it existed in the layout.
    */
  def removeRoom(room: Room): MapLayout[S]

  /**
    * @param room Room to check removal status for.
    * @return True if the room is currently not in the map, otherwise false.
    */
  def isRemoved(room: Room): Boolean

  /**
    * @param room Room in the layout.
    * @return True if the room with the specified id ID was added to connect other rooms, otherwise false.
    */
  def isHub(room: Room): Boolean

  /**
    * @param room Room in the layout.
    * @return Every opening connected to the room with the specified id. Empty if the room is deleted.
    */
  def openingsFor(room: Room): Vector[Opening]

}
