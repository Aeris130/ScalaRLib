package rldungeon.help

import net.cyndeline.rlcommon.util.IDPool
import net.cyndeline.scalarlib.rldungeon.common.PointlessLevel
import net.cyndeline.scalarlib.rldungeon.levelPath.TreePath

import scalax.collection.immutable.Graph

/**
 * Test level. Starts producing id's at the highest ID already found in the initial graph.
 *
 * To save work whenever a level is created, every room and corridor is set to return reward/activator/responder
 * capacity 1 by default.
 */
class GraphLevel private (val asGraph: Graph[RoomVertex, CorridorEdge],
                          vertexIds: IDPool,
                          strt: Option[RoomVertex],
                          gl: Option[RoomVertex],
                          override val responderAmount: Int,
                          val activatorAndResponders: Vector[(Set[RoomVertex], CorridorEdge[RoomVertex])],
                          val rewards: Vector[RoomVertex],
                          val activatorCapacity: Map[RoomVertex, Int],
                          val responderCapacity: Map[CorridorEdge[RoomVertex], Int],
                          val rewardCapacity: Map[RoomVertex, Int],
                          val pointlessArea: Set[Set[RoomVertex]],
                          val mainPath: Option[TreePath]) extends PointlessLevel[GraphLevel, RoomVertex, CorridorEdge] {

  def this(g: Graph[RoomVertex, CorridorEdge], ids: IDPool) = this(g, ids, None, None, 0, Vector(), Vector(), Map(), Map(), Map(), Set(), None)

  def setStart(r: RoomVertex) = new GraphLevel(asGraph, vertexIds, Some(r), gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  def setGoal(r: RoomVertex) = new GraphLevel(asGraph, vertexIds, strt, Some(r), responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  def setRewardCapacity(r: RoomVertex, c: Int) = new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity + (r -> c), pointlessArea, mainPath)
  def setActivatorCapacity(r: RoomVertex, c: Int) = new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity + (r -> c), responderCapacity, rewardCapacity, pointlessArea, mainPath)
  def setResponderCapacity(edge: CorridorEdge[RoomVertex], c: Int) = new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity + (edge -> c), rewardCapacity, pointlessArea, mainPath)
  def setResponderAmount(amount: Int) = new GraphLevel(asGraph, vertexIds, strt, gl, amount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)

  /**
   * Creates a new room.
   * @return The room that was created, as well as a copy of this level without the room added. Remember to use
   *         addRoom if the room is to be added to the level. The reason for returning the level without the room
   *         is to allow updates of internal data structures, such as ID pools.
   */
  override def createRoom: (RoomVertex, GraphLevel) = {
    val nextVertexId = vertexIds.nextId
    val vertex = new RoomVertex(nextVertexId._1)
    val level = new GraphLevel(asGraph, nextVertexId._2, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
    (vertex, level)
  }

  /**
   * @param room Room to add to the level.
   * @return An updated copy of the level with the room added.
   */
  override def addRoom(room: RoomVertex): GraphLevel = {
    new GraphLevel(asGraph + room, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  /**
   * @param room A room in the level to delete.
   * @return An updated copy of the level with the room deleted.
   */
  override def deleteRoom(room: RoomVertex): GraphLevel = {
    val updatedIds = vertexIds.deleteID(room.rid)
    new GraphLevel(asGraph - room, updatedIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  /**
   * Adds a connection between two rooms in the level.
   * @param from The first room to connect.
   * @param to The second room to connect.
   * @return A copy of this level with the connection added.
   */
  override def connectRooms(from: RoomVertex, to: RoomVertex): GraphLevel = {
    new GraphLevel(asGraph + CorridorEdge(from, to), vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  /**
   * Removes a connection between two connected rooms in the level.
   * @param from The first room to disconnect.
   * @param to The second room to disconnect.
   * @return A copy of this level with the connection removed.
   */
  override def disconnectRooms(from: RoomVertex, to: RoomVertex): GraphLevel = {
    new GraphLevel(asGraph - CorridorEdge(from, to), vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  override def equals(other: Any): Boolean = other match {
    case gl: GraphLevel => gl.asGraph == asGraph
    case _ => false
  }

  override def hashCode: Int = asGraph.##

  // Pointless area methods

  /** @return The room where the player enters the level. */
  override def start: RoomVertex = strt.getOrElse(throw new Error("No start room set"))

  /** @return The room where the player exists the level. */
  override def goal: RoomVertex = gl.getOrElse(throw new Error("No goal room set"))

  /**
   * Marks rooms as a recipient of a reward.
   * @param rooms Rooms to assign rewards to. Each room will only appear once in this collection, adding multiple
   *              rewards to the same room will be done by calling the method multiple times.
   * @return A copy of this level with the specified rooms marked as receiving a reward (one each).
   */
  override def addRewards(rooms: Vector[RoomVertex]): GraphLevel = {
    new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards ++ rooms, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  /**
   * @param room A room in the level.
   * @return The amount of additional activators that the room may be assigned.
   */
  override def remainingActivatorCapacity(room: RoomVertex): Int = {
    activatorCapacity.get(room).getOrElse(1)
  }

  /**
   * @param room A room in the level.
   * @return The amount of additional rewards that the room may be assigned.
   */
  override def remainingRewardCapacity(room: RoomVertex): Int = {
    rewardCapacity.get(room).getOrElse(1)
  }

  /**
   * @return The number of responders that the input connection can carry initially. If no responders can be carried,
   *         0 should be returned.
   */
  override def remainingResponderCapacity(connection: CorridorEdge[RoomVertex]): Int = {
    responderCapacity.get(connection).getOrElse(1)
  }

  override def addActivatorAndResponder(rooms: Set[RoomVertex], connection: CorridorEdge[RoomVertex]): GraphLevel = {
    new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders :+ (rooms -> connection), rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, mainPath)
  }

  override def markMainPath(path: TreePath): GraphLevel = {
    new GraphLevel(asGraph, vertexIds, strt, gl, responderAmount, activatorAndResponders, rewards, activatorCapacity, responderCapacity, rewardCapacity, pointlessArea, Some(path))
  }

}

object GraphLevel {
  def apply(graph: Graph[RoomVertex, CorridorEdge]): GraphLevel = apply(graph, false)
  def apply(graph: Graph[RoomVertex, CorridorEdge], start: RoomVertex, goal: RoomVertex): GraphLevel = apply(graph, false).setStart(start).setGoal(goal)
  def freeUnusedIds(graph: Graph[RoomVertex, CorridorEdge]): GraphLevel = apply(graph, true)
  def apply(graph: Graph[RoomVertex, CorridorEdge], freeUnusedIds: Boolean): GraphLevel = {
    val idSet = graph.nodes.map(_.rid).toSet
    val highestId = if (graph.isEmpty) 0 else idSet.max + 1
    var pool = new IDPool(highestId)

    if (freeUnusedIds) {
      for (i <- 0 to highestId if !idSet.contains(i))
        pool = pool.deleteID(i)
    }
    
    new GraphLevel(graph, pool)
  }
}
