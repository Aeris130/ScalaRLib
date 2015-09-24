package net.cyndeline.scalarlib.rldungeon.common

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Extended by objects representing a single level. Note to users: Multiple copies produced by the implementing
 * class may be accessed and modified independently by the algorithms. The levels produced by the methods in this trait
 * must either be immutable or deep copies.
 *
 * @tparam L Class type implementing this trait.
 * @tparam R Class type representing rooms in the level.
 * @tparam C Class type representing corridors/connections in the level.
 */
trait Level[L <: Level[L, R, C], R, C[X] <: UnDiEdge[X]] {

  /**
   * @return The level represented as a graph. Any two rooms that allows traversal between them should be connected
   *         by an edge.
   */
  def asGraph: Graph[R, C]

  /**
   * Creates a new room.
   * @return The room that was created, as well as a copy of this level without the room added. Remember to use
   *         addRoom if the room is to be added to the level. The reason for returning the level without the room
   *         is to allow updates of internal data structures, such as ID pools.
   */
  def createRoom: (R, L)

  /**
   * @param room Room to add, should not currently exist inside the level.
   * @return An updated copy of the level with the room added.
   */
  def addRoom(room: R): L

  /**
   * @param room A room in the level to delete.
   * @return An updated copy of the level with the room deleted.
   */
  def deleteRoom(room: R): L

  /**
   * Adds a connection between two rooms in the level.
   * @param from The first room to connect.
   * @param to The second room to connect.
   * @return A copy of this level with the connection added.
   */
  def connectRooms(from: R, to: R): L

  /**
   * Removes a connection between two connected rooms in the level.
   * @param from The first room to disconnect.
   * @param to The second room to disconnect.
   * @return A copy of this level with the connection removed.
   */
  def disconnectRooms(from: R, to: R): L

}
