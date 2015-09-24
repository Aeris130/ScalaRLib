package net.cyndeline.scalarlib.rldungeon.grammar.util

import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Generic ComponentProduction that modifies the topology of a graph. Supports:
 *
 *  # Adding vertices
 *  # Removing vertices (all connected edges are deleted)
 *  # Adding edges A ~ B where A and B are either a previous vertex, or a newly added vertex
 *  # Deleting edges A ~ B (A and B remains in the graph)
 *
 * Operations are performed in the following order: Vertex add, edge add, edge remove, vertex remove.
 */
class TopologyProduction[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] private
    (vertexAdd: Set[Int],
     vertexRemove: Set[R],
     newEdgeAdd: Set[(Int, Int)],
     oldEdgeAdd: Set[(R, R)],
     mixEdgeAdd: Set[(Int, R)],
     oldEdgeRemove: Set[(R, R)],
     newRoomModifications: Map[Int, RoomModification[R]]) extends ComponentProduction[L, R, C] {

  noOverlappingVerticesBetweenVertexRemovalAndEdgeAdditionAllowed()
  addingAndRemovingAnEdgeException()
  modificationIdsExistException()

  /**
   * @constructor Creates an empty topology production that doesn't modify a levels topology.
   */
  def this() = this(Set(), Set(), Set(), Set(), Set(), Set(), Map())

  /**
   * Performs a modification of a level.
   *
   * @param morphism Maps a set of vertices in a production pattern to the vertices in the levels sub graph. 
   *                 It's up to the ComponentProductions implementation to know which vertices to use as keys here.
   * @param level The level that the sub graph appears in, in its entirety. Every room here is not guaranteed to
   *              appear in the morphism. This object is only supplied to allow ComponentProductions to perform
   *              level-spanning algorithms that doesn't rely on the content of the pattern, as well as giving them the
   *              level as it appears before modification.
   *              NOTE: Do not use this object to check if it is valid for a particular modification, put
   *              those checks in the negative condition inside the Production class.
   * @return a copy of the input level, modified by this production.
   */
  override def apply(morphism: Morphism[R], level: L): L = {
    var addedVertices = Map[Int, R]()
    var finalLevel = level
    val verticesToAdd = vertexAdd.iterator

    while (verticesToAdd.hasNext) {
      val vertexId: Int = verticesToAdd.next()
      val vertexProduction = finalLevel.createRoom
      val newRoom = vertexProduction._1
      finalLevel = vertexProduction._2

      val newVertex = if (newRoomModifications.contains(vertexId))
        newRoomModifications(vertexId).modify(newRoom)
      else
        newRoom
      addedVertices += (vertexId -> newVertex)

      finalLevel = finalLevel.addRoom(newVertex)
    }

    finalLevel = addNewEdges(newEdgeAdd, addedVertices, finalLevel)
    finalLevel = addOldEdges(oldEdgeAdd, morphism, finalLevel)
    finalLevel = addMixEdges(mixEdgeAdd, addedVertices, morphism, finalLevel)
    finalLevel = removeEdges(oldEdgeRemove, morphism, level.asGraph, finalLevel)

    val verticesToRemove = vertexRemove.iterator
    while (verticesToRemove.hasNext) {
      val vertexInGraph = morphism.getVertexCorrespondingTo(verticesToRemove.next())
      finalLevel = finalLevel.deleteRoom(vertexInGraph)
    }

    finalLevel
  }

  /**
   * Creates a new vertex.
   * @param id Numeric identifier for the new vertex. This id will not be used in the graph, but is a way to reference
   *           the vertex being created when adding additional modifications (such as edge additions using the vertex).
   * @return a new TopologyProduction that adds an additional vertex to the input graph.
   */
  def addVertex(id: Int): TopologyProduction[L, R, C] = {
    if (!vertexAdd.contains(id))
      new TopologyProduction[L, R, C](vertexAdd + id, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("The vertex id " + id + " was used twice.")
  }

  /**
   * Removes a vertex already present in the input graph.
   * @param v Vertex in the graph pattern representing the vertex to remove.
   * @return a new TopologyProduction that removes the vertex corresponding to the specified vertex in the morphism.
   */
  def removeVertex(v: R): TopologyProduction[L, R, C] = {
    new TopologyProduction[L, R, C](vertexAdd, vertexRemove + v, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
  }

  /**
   * Adds an edge between two vertices already present in the graph.
   * @param from A vertex in the graph.
   * @param to Another vertex in the graph.
   * @return a new TopologyProduction that adds an edge between the specified vertices in the morphism.
   */
  def addOldEdge(from: R, to: R): TopologyProduction[L, R, C] = {
    if (from != to)
      new TopologyProduction[L, R, C](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd + ((from, to)), mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("Cannot add edges to and from the same vertex (" + from + ")")
  }

  /**
   * Adds an edge between a previous vertex and a vertex added by this production.
   * @param from The id representing the newly added vertex.
   * @param to An old vertex in the graph morphism.
   * @return a new TopologyProduction that adds an edge between the specified vertices.
   */
  def addMixEdge(from: Int, to: R): TopologyProduction[L, R, C] = {
    new TopologyProduction[L, R, C](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd + ((from, to)), oldEdgeRemove, newRoomModifications)
  }

  /**
   * Adds an edge between two vertices added by this production.
   * @param from The id representing a newly added vertex.
   * @param to The id representing another newly added vertex. Must differ from the other vertex.
   * @return a new TopologyProduction that adds an edge between the specified vertices.
   */
  def addNewEdge(from: Int, to: Int): TopologyProduction[L, R, C] = {
    if (from != to)
      new TopologyProduction[L, R, C](vertexAdd, vertexRemove, newEdgeAdd + ((from, to)), oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("Cannot add edges to and from the same vertex (" + from + ")")
  }

  /**
   * Removes an edge already present in the graph.
   * @param from A vertex in the graph.
   * @param to Another vertex in the graph.
   * @return a new TopologyProduction that removes an edge between the specified vertices.
   */
  def removeEdge(from: R, to: R): TopologyProduction[L, R, C] = {
    new TopologyProduction[L, R, C](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove + ((from, to)), newRoomModifications)
  }

  /**
   * Sets modification instructions for a new vertex to be created.
   * @param newVertexId Id of the created vertex (must already be in the production).
   * @param modification Modifications to apply to the vertex.
   * @return A new topology that modifies some of the vertices after creating it.
   */
  def addModification(newVertexId: Int, modification: RoomModification[R]) = {
    new TopologyProduction[L, R, C](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications + (newVertexId -> modification))
  }

  private def addNewEdges(newEdgeAdd: Set[(Int, Int)],
                          addedVertices: Map[Int, R],
                          level: L): L = {
    var newLevel = level
    val newEdgesToAdd = newEdgeAdd.iterator
    while (newEdgesToAdd.hasNext) {
      val edge = newEdgesToAdd.next()
      val from = addedVertices(edge._1)
      val to = addedVertices(edge._2)
      newLevel = newLevel.connectRooms(from, to)
    }

    newLevel
  }

  private def addOldEdges(oldEdgeAdd: Set[(R, R)],
                          morphism: Morphism[R],
                          level: L): L = {
    var newLevel = level
    val oldEdgesToAdd = oldEdgeAdd.iterator
    while (oldEdgesToAdd.hasNext) {
      val edges = oldEdgesToAdd.next()
      val from = morphism.getVertexCorrespondingTo(edges._1)
      val to = morphism.getVertexCorrespondingTo(edges._2)
      newLevel = newLevel.connectRooms(from, to)
    }

    newLevel
  }

  private def addMixEdges(mixEdgeAdd: Set[(Int, R)],
                          addedVertices: Map[Int, R],
                          morphism: Morphism[R],
                          level: L): L = {
    var newLevel = level
    val mixEdgesToAdd = mixEdgeAdd.iterator
    while(mixEdgesToAdd.hasNext) {
      val edge = mixEdgesToAdd.next()
      val from = addedVertices(edge._1)
      val to = morphism.getVertexCorrespondingTo(edge._2)
      newLevel = newLevel.connectRooms(from, to)
    }

    newLevel
  }

  private def removeEdges(oldEdgeRemove: Set[(R, R)],
                          morphism: Morphism[R],
                          totalGraph: Graph[R, C],
                          level: L): L = {
    var newLevel = level
    val oldEdgesToRemove = oldEdgeRemove.iterator
    while (oldEdgesToRemove.hasNext) {
      val vertexPair = oldEdgesToRemove.next()
      val a = morphism.getVertexCorrespondingTo(vertexPair._1)
      val b = morphism.getVertexCorrespondingTo(vertexPair._2)
      require(GraphCommons.neighbors(a, totalGraph).contains(b), "Cannot remove edge, the initial level did not contain an edge from " + a + " to " + b + ".")
      newLevel = newLevel.disconnectRooms(a, b)
    }

    newLevel
  }

  /**
   * Throws an exception if the production attempts to both add an edge to a previous vertex, as well as remove it.
   */
  private def noOverlappingVerticesBetweenVertexRemovalAndEdgeAdditionAllowed(): Unit = {
    var verticesInvolvedInEdgeRemoval = Set[R]()
    oldEdgeAdd.foreach(fromTo => {
      verticesInvolvedInEdgeRemoval += fromTo._1
      verticesInvolvedInEdgeRemoval += fromTo._2
    })

    mixEdgeAdd.foreach(newToOld => verticesInvolvedInEdgeRemoval += newToOld._2)

    val overlapping = vertexRemove.intersect(verticesInvolvedInEdgeRemoval)
    if (!overlapping.isEmpty)
      throw new IllegalArgumentException("Vertices " + overlapping.mkString(", ") + " cannot both be removed and involved in edge construction. Edges connected to a removed vertex will also be removed.")
  }

  /**
   * Throws an exception if the production attempts to both add and remove an edge between two old vertices.
   */
  private def addingAndRemovingAnEdgeException() {
    var vertexPairs = Set[(R, R)]()
    oldEdgeAdd.foreach(fromTo => {
      vertexPairs += ((fromTo._1, fromTo._2))
      vertexPairs += ((fromTo._2, fromTo._1))
    })

    val edgeRemoval = oldEdgeRemove.iterator
    while (edgeRemoval.hasNext) {
      val pair = edgeRemoval.next()
      if (vertexPairs.contains((pair._1, pair._2)))
        throw new IllegalArgumentException("Attempted to both add and remove an edge between " + pair._1 + " and " + pair._2)
    }
  }

  private def modificationIdsExistException(): Boolean = {
    if (!newRoomModifications.isEmpty && !newRoomModifications.keySet.subsetOf(vertexAdd))
      throw new Error("Cannot add modifications to ids that haven't been added yet: " + vertexAdd.diff(newRoomModifications.keySet).mkString(", "))

    true
  }
}
