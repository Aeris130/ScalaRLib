package net.cyndeline.scalarlib.rldungeon.grammar.util

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common._
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import scala.language.higherKinds
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.immutable.Graph

/**
 * Generic ComponentProduction that modifies the topology of a graph. Supports:
 *
 *  - Adding vertices
 *  - Removing vertices (all connected edges are deleted)
 *  - Adding edges A ~ B where A and B are either a previous vertex, or a newly added vertex. Edges may be directed or
 *  undirected.
 *  - Deleting edges A ~ B (A and B remains in the graph). Edges may be directed or undirected.
 *
 * Operations are performed in the following order: Vertex add, edge add, edge remove, vertex remove.
 */
class TopologyProduction[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV] private
    (vertexAdd: Set[Int],
     vertexRemove: Set[PV],
     newEdgeAdd: Set[TopologyProduction.NewEdge],
     oldEdgeAdd: Set[TopologyProduction.OldEdge[PV]],
     mixEdgeAdd: Set[TopologyProduction.MixedEdge[PV]],
     oldEdgeRemove: Set[TopologyProduction.OldEdge[PV]],
     newRoomModifications: Map[Int, RoomModification[R]]) extends ComponentProduction[L, R, C, PV] {

  noOverlappingVerticesBetweenVertexRemovalAndEdgeAdditionAllowed()
  addingRemovingSameUndirected()
  addingRemovingSameDirected()
  modificationIdsExistException()
  duplicateNewEdges(true)
  duplicateNewEdges(false)
  intersecteNewUndirectedAndDirected()

  /**
   * Creates an empty topology production that doesn't modify a levels topology.
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
  override def apply(morphism: Morphism[R, PV], level: L): L = {
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

    finalLevel = removeEdges(oldEdgeRemove, morphism, level.asGraph, finalLevel)
    finalLevel = addNewEdges(newEdgeAdd, addedVertices, finalLevel)
    finalLevel = addOldEdges(oldEdgeAdd, morphism, finalLevel)
    finalLevel = addMixEdges(mixEdgeAdd, addedVertices, morphism, finalLevel)


    val verticesToRemove = vertexRemove.iterator
    while (verticesToRemove.hasNext) {
      val vertexInGraph = morphism.matching(verticesToRemove.next())
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
  def addVertex(id: Int): TopologyProduction[L, R, C, PV] = {
    if (!vertexAdd.contains(id))
      new TopologyProduction[L, R, C, PV](vertexAdd + id, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("The vertex id " + id + " was used twice.")
  }

  /**
   * Removes a vertex already present in the input graph.
   * @param v Vertex in the graph pattern representing the vertex to remove.
   * @return a new TopologyProduction that removes the vertex corresponding to the specified vertex in the morphism.
   */
  def removeVertex(v: PV): TopologyProduction[L, R, C, PV] = {
    new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove + v, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
  }

  /**
   * Adds an edge between two vertices already present in the graph.
   * @param from A vertex in the graph.
   * @param to Another vertex in the graph.
   * @param direction Specifies if the edge should be directed to->from, or if it should be undirected.
   * @return a new TopologyProduction that adds an edge between the specified vertices in the morphism.
   */
  def addOldEdge(from: PV, to: PV, direction: CorridorDirection = Undirected): TopologyProduction[L, R, C, PV] = {
    if (from != to)
      new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd + new TopologyProduction.OldEdge[PV](from, to, direction), mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("Cannot add edges to and from the same vertex (" + from + ")")
  }

  /**
   * Adds an edge between a vertex added by this production and a previous vertex.
   * @param from The id representing the newly added vertex.
   * @param to An old vertex in the graph morphism.
   * @param direction Specifies if the edge should be directed to->from, or if it should be undirected.
   * @return a new TopologyProduction that adds an edge between the specified vertices.
   */
  def addMixEdge(from: Int, to: PV, direction: CorridorDirection = Undirected): TopologyProduction[L, R, C, PV] = {
    val entry = new TopologyProduction.MixedEdge[PV](from, to, direction, true)
    new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd + entry, oldEdgeRemove, newRoomModifications)
  }

  /**
   * Adds an edge between a previous vertex and a vertex added by this production and .
   * @param from An old vertex in the graph morphism.
   * @param to The id representing the newly added vertex.
   * @param direction Specifies if the edge should be directed to->from, or if it should be undirected.
   * @return a new TopologyProduction that adds an edge between the specified vertices.
   */
  def addMixEdge(from: PV, to: Int, direction: CorridorDirection): TopologyProduction[L, R, C, PV] = {
    val entry = new TopologyProduction.MixedEdge[PV](to, from, direction, false)
    new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd + entry, oldEdgeRemove, newRoomModifications)
  }

  /**
   * Adds an edge between two vertices added by this production.
   * @param from The id representing a newly added vertex.
   * @param to The id representing another newly added vertex. Must differ from the other vertex.
   * @param direction Specifies if the edge should be directed to->from, or if it should be undirected.
   * @return a new TopologyProduction that adds an edge between the specified vertices.
   */
  def addNewEdge(from: Int, to: Int, direction: CorridorDirection = Undirected): TopologyProduction[L, R, C, PV] = {
    if (from != to)
      new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd + new TopologyProduction.NewEdge(from, to, direction), oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications)
    else
      throw new IllegalArgumentException("Cannot add edges to and from the same vertex (" + from + ")")
  }

  /**
   * Removes an edge already present in the graph. If the specified vertices contains a directed edge from->to, that
   * edge will be removed. Otherwise the undirected edge from~to will be removed.
   * @param from A vertex in the graph.
   * @param to Another vertex in the graph.
   * @param direction Whether the edge to be removed is directed from->to, or undirected. This parameter is mostly used
   *                  to make it unambiguous in regards to what type of edge is being removed, as it helps the
   *                  production to catch user-errors during setup.
   * @return a new TopologyProduction that removes an edge between the specified vertices.
   */
  def removeEdge(from: PV, to: PV, direction: CorridorDirection): TopologyProduction[L, R, C, PV] = {
    val entry = new TopologyProduction.OldEdge[PV](from, to, direction)
    new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove + entry, newRoomModifications)
  }

  /**
   * Sets modification instructions for a new vertex to be created.
   * @param newVertexId Id of the created vertex (must already be in the production).
   * @param modification Modifications to apply to the vertex.
   * @return A new topology that modifies some of the vertices after creating it.
   */
  def addModification(newVertexId: Int, modification: RoomModification[R]) = {
    new TopologyProduction[L, R, C, PV](vertexAdd, vertexRemove, newEdgeAdd, oldEdgeAdd, mixEdgeAdd, oldEdgeRemove, newRoomModifications + (newVertexId -> modification))
  }

  private def addNewEdges(newEdgeAdd: Set[TopologyProduction.NewEdge],
                          addedVertices: Map[Int, R],
                          level: L): L = {
    var newLevel = level
    val newEdgesToAdd = newEdgeAdd.iterator
    while (newEdgesToAdd.hasNext) {
      val edge = newEdgesToAdd.next()
      val from = addedVertices(edge._1)
      val to = addedVertices(edge._2)
      val direction = if (edge.isDirected) Directed else Undirected
      newLevel = newLevel.connectRooms(from, to, direction)
    }

    newLevel
  }

  private def addOldEdges(oldEdgeAdd: Set[TopologyProduction.OldEdge[PV]],
                          morphism: Morphism[R, PV],
                          level: L): L = {
    var newLevel = level
    val oldEdgesToAdd = oldEdgeAdd.iterator
    while (oldEdgesToAdd.hasNext) {
      val edge = oldEdgesToAdd.next()
      val from = morphism.matching(edge._1)
      val to = morphism.matching(edge._2)
      val direction = if (edge.isDirected) Directed else Undirected
      newLevel = newLevel.connectRooms(from, to, direction)
    }

    newLevel
  }

  private def addMixEdges(mixEdgeAdd: Set[TopologyProduction.MixedEdge[PV]],
                          addedVertices: Map[Int, R],
                          morphism: Morphism[R, PV],
                          level: L): L = {
    var newLevel = level
    val mixEdgesToAdd = mixEdgeAdd.iterator
    while(mixEdgesToAdd.hasNext) {
      val edge = mixEdgesToAdd.next()
      val from = addedVertices(edge._1)
      val to = edge._2

      if (!edge.isDirected) {
        newLevel = newLevel.connectRooms(from, morphism.matching(to))
      } else if (edge.startsAtNew) {
        newLevel = newLevel.connectRooms(from, morphism.matching(to), Directed)
      } else {
        newLevel = newLevel.connectRooms(morphism.matching(to), from, Directed)
      }

    }

    newLevel
  }

  private def removeEdges(oldEdgeRemove: Set[TopologyProduction.OldEdge[PV]],
                          morphism: Morphism[R, PV],
                          totalGraph: Graph[R, C],
                          level: L): L = {
    var newLevel = level
    val oldEdgesToRemove = oldEdgeRemove.iterator
    while (oldEdgesToRemove.hasNext) {
      val vertexPair = oldEdgesToRemove.next()
      val a = morphism.matching(vertexPair._1)
      val b = morphism.matching(vertexPair._2)
      require(GraphCommons.neighbors(a, totalGraph).contains(b), "Cannot remove edge, the initial level did not contain an edge from " + a + " to " + b + ".")
      newLevel = newLevel.disconnectRooms(a, b, vertexPair.direction)
    }

    newLevel
  }

  /**
   * Throws an exception if the production attempts to both add an edge to a previous vertex, as well as remove it.
   */
  private def noOverlappingVerticesBetweenVertexRemovalAndEdgeAdditionAllowed(): Unit = {
    var verticesInvolvedInEdgeRemoval = Set[PV]()
    oldEdgeAdd.foreach(fromTo => {
      verticesInvolvedInEdgeRemoval += fromTo._1
      verticesInvolvedInEdgeRemoval += fromTo._2
    })

    mixEdgeAdd.foreach(newToOld => verticesInvolvedInEdgeRemoval += newToOld._2)

    val overlapping = vertexRemove.intersect(verticesInvolvedInEdgeRemoval)
    if (overlapping.nonEmpty)
      throw new IllegalArgumentException("Vertices " + overlapping.mkString(", ") + " cannot both be removed and involved in edge construction. Edges connected to a removed vertex will also be removed.")
  }

  /**
   * Throws an exception if the production attempts to both add and remove an undirected edge between two old vertices.
   */
  private def addingRemovingSameUndirected(): Unit = {
    val edgesToAdd = oldEdgeAdd.filter(!_.isDirected).map(e => UnorderedPair(e._1, e._2))
    for (edge <- oldEdgeRemove if !edge.isDirected)
      require(!edgesToAdd.contains(UnorderedPair(edge._1, edge._2)), "Attempted to both add and remove an undirected edge between " + edge._1 + " and " + edge._2)
  }

  /**
   * Throws an exception if the production attempts to both add and remove a directed edge between two old vertices.
   */
  private def addingRemovingSameDirected(): Unit = {
    val edgesToAdd = oldEdgeAdd.filter(_.isDirected).map(e => (e._1, e._2))
    for (edge <- oldEdgeRemove if edge.isDirected)
      require(!edgesToAdd.contains((edge._1, edge._2)), "Attempted to both add and remove a directed edge between " + edge._1 + " and " + edge._2)
  }

  private def modificationIdsExistException(): Boolean = {
    if (newRoomModifications.nonEmpty && !newRoomModifications.keySet.subsetOf(vertexAdd))
      throw new IllegalArgumentException("Cannot add modifications to ids that haven't been added yet: " + vertexAdd.diff(newRoomModifications.keySet).mkString(", "))

    true
  }

  private def duplicateNewEdges(directed: Boolean): Unit = {
    val allPairs = newEdgeAdd.toVector.filter(_.isDirected == directed).map(e => (e._1, e._2))
    require(allPairs.distinct.size == allPairs.size, "Cannot add a directed edge twice.")
  }

  private def intersecteNewUndirectedAndDirected(): Unit = {
    val allDirected = newEdgeAdd.toVector.filter(_.isDirected).map(e => (e._1, e._2))
    val allUndirected = newEdgeAdd.toVector.filter(!_.isDirected).map(e => (e._1, e._2))
    require(allDirected.intersect(allUndirected).isEmpty, "An undirected and a directed edge connects to the same vertex pair.")
  }

}

private object TopologyProduction {
  private class NewEdge(val _1: Int, val _2: Int, val direction: CorridorDirection) {
    val isDirected = direction == Directed
    override def toString: String = {
      val builder = new StringBuilder()
      builder ++= _1.toString
      if (isDirected) builder ++= " ~> " else builder ++= " ~ "
      builder ++= _2.toString
      builder.toString()
    }
  }

  private class OldEdge[PV](val _1: PV, val _2: PV, val direction: CorridorDirection) {
    val isDirected = direction == Directed
    override def toString: String = {
      val builder = new StringBuilder()
      builder ++= _1.toString
      if (isDirected) builder ++= " ~> " else builder ++= " ~ "
      builder ++= _2.toString
      builder.toString()
    }
  }

  private class MixedEdge[PV](val _1: Int, val _2: PV, val direction: CorridorDirection, val startsAtNew: Boolean) {
    val isDirected = direction == Directed
    override def toString: String = {
      val builder = new StringBuilder()
      if (startsAtNew) builder ++= _1.toString else _2.toString
      if (isDirected) builder ++= " ~> " else builder ++= " ~ "
      if (startsAtNew) builder ++= _2.toString else _1.toString
      builder.toString()
    }
  }
}
