package net.cyndeline.scalarlib.rldrawing.forceGrid

import java.util.concurrent.TimeUnit

import net.cyndeline.rlcommon.math.geom.{Dimensions, Point, Rectangle}
import net.cyndeline.rlcommon.math.geom.spatialIndex.kdTree.KDTree
import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldrawing.common.{Connection, ConnectionData, RRoom, Room}
import net.cyndeline.scalarlib.rldrawing.{MapLayout, Opening}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._

/**
  * Stores the original rooms specified by the user, as well as additional hub rooms generated by the algorithm.
  * Removing an edge connected to a hub will also remove the hub if no other edges exist to it.
  *
  * @param hubsFrom The room index for the first hub. Every room past it will also be a hub.
  */
//TODO handle large number of objects by building connections and openings on the fly. Only store modified data.
//TODO Check that openings are constructed properly on single-coordinate openings that occur in corners
//TODO Check that connections and openings aren't returned if they're deleted
class ForceGridLayout private (override val rooms: Vector[RRoom],
                               override val connectionGraph: Graph[Int, UnDiEdge],
                               dimensions: Dimensions,
                               hubsFrom: Int) extends MapLayout[RRoom]() {

  override def createConnection(data: ConnectionData): MapLayout[RRoom] = ???

  override def modifyConnection(connection: Connection, update: ConnectionData): MapLayout[RRoom] = ???

  override def addConnection(connection: Connection): MapLayout[RRoom] = ???

  override def removeConnection(connection: Connection): MapLayout[RRoom] = ???

  override def connectionsFor(room: Room): Vector[Connection] = buildConnection(room)

  override def addRoom(room: Room): MapLayout[RRoom] = ???

  override def removeRoom(room: Room): MapLayout[RRoom] = ???

  override def isRemoved(room: Room): Boolean = ???

  override def isHub(room: Room): Boolean = room.id >= hubsFrom

  override def openingsFor(room: Room): Vector[Opening] = buildOpenings(room)

  override def width: Int = dimensions.width

  override def height: Int = dimensions.height

  //TODO implement!
  private def buildConnection(room: Room): Vector[Connection] = ???

  private def buildOpenings(room: Room): Vector[Opening] = {
    val neighbors = GraphCommons.outerNeighbors(room.id, connectionGraph)
    neighbors.map(n => constructOpening(rooms(room.id), room.id, rooms(n), n))
  }

  private def constructOpening(r1: Rectangle, r1Id: Int,  r2: Rectangle, r2Id: Int): Opening = {
    val intersection = r1.intersection(r2).get
    val xAdjust = intersection.width > 1
    val startP = if (xAdjust) intersection.start + (1, 0) else intersection.start + (0, 1)
    val stopP = if (xAdjust) intersection.stop - (1, 0) else intersection.stop - (0, 1)
    Opening(r1Id, r2Id, Vector((startP, stopP)))
  }

}

/**
  * Creates layouts. Note that when no connection graph is supplied, the factory will compute one on its own which may
  * be computationally expensive for maps with a large number of rooms.
  */
object ForceGridLayout {
  def apply(areas: Vector[Rectangle]): ForceGridLayout = apply(areas, Vector())
  def apply(areas: Vector[Rectangle], hubs: Vector[Rectangle]): ForceGridLayout = construct(areas, hubs, None)

  def empty = apply(Vector[Rectangle]())

  /**
    * @param areas Every area on the map.
    * @param hubs Areas connecting other areas together.
    * @param connections Uses the indices in the area vector as ids, and stores an edge between two ids if the two
    *                    areas share a border such that traversing between them is possible.
    * @return The final layout.
    */
  def withConnectionGraph(areas: Vector[Rectangle], hubs: Vector[Rectangle], connections: Graph[Int, UnDiEdge]): ForceGridLayout = construct(areas, hubs, Some(connections))

  private def construct(areas: Vector[Rectangle], hubs: Vector[Rectangle], connectionGraph: Option[Graph[Int, UnDiEdge]]): ForceGridLayout = {
    val allAreas = areas ++ hubs
    val adjusted = if (allAreas.isEmpty) allAreas else adjustToZero(allAreas)
    val rooms = adjusted.zipWithIndex.map(ai => RRoom(ai._2, ai._1))

    // Create openings and connections
    val connections = connectionGraph.getOrElse(constructConnectionGraph(rooms))

    new ForceGridLayout(rooms, connections, Dimensions(width(rooms), height(rooms)), areas.length)
  }

  private def width(rooms: Vector[Rectangle]): Int = if (rooms.isEmpty) {
    0
  } else {
    val r = rooms.maxBy(r => r.start.x + r.width)
    r.start.x + r.width
  }

  private def height(rooms: Vector[Rectangle]): Int = if (rooms.isEmpty) {
    0
  } else {
    val r = rooms.maxBy(r => r.start.y + r.height)
    r.start.y + r.height
  }

  private def adjustToZero(rooms: Vector[Rectangle]) = {
    val startCoordinates = rooms.map(_.start)
    val xAdjust = startCoordinates.minBy(_.x).x * -1
    val yAdjust = startCoordinates.minBy(_.y).y * -1
    rooms.map(r => r + (xAdjust, yAdjust))
  }

  private def constructConnectionGraph(rooms: Vector[Rectangle]): Graph[Int, UnDiEdge] = {
    val roomToId = rooms.zipWithIndex.toMap
    val kdTree = KDTree.rectangleTree(rooms)
    val connectionEdges = new ArrayBuffer[UnDiEdge[Int]]()

    val roomIt = rooms.iterator
    while (roomIt.hasNext) {
      val room = roomIt.next()
      val id = roomToId(room)
      val neighbors = kdTree.rangeSearch(room) // Gets the rooms that share an edge with this one
      val neighborIt = neighbors.iterator
      while (neighborIt.hasNext) {
        val n = neighborIt.next()

        if (n != room) {
          val nid = roomToId(n)
          val intersection = room.intersection(n)
            .getOrElse(throw new Error(s"Attempted to create opening between non-intersecting rectangles $room and $n"))

          if (intersection.width > 2 || intersection.height > 2) {
            connectionEdges += id~nid
          }
        }

      }
    }

    Graph.from(Nil, connectionEdges)
  }

  private def constructOpenings(rooms: Vector[Rectangle], connections: Graph[Int, UnDiEdge]): Vector[Opening] = {
    val edges = GraphCommons.outerEdges(connections)
    edges.map(e => constructOpening(rooms(e._1), e._1, rooms(e._2), e._2))
  }

  private def constructOpening(r1: Rectangle, r1Id: Int,  r2: Rectangle, r2Id: Int): Opening = {
    val intersection = r1.intersection(r2).get
    val xAdjust = intersection.width > 1
    val startP = if (xAdjust) intersection.start + (1, 0) else intersection.start + (0, 1)
    val stopP = if (xAdjust) intersection.start - (1, 0) else intersection.start - (0, 1)
    Opening(r1Id, r2Id, Vector((startP, stopP)))
  }

  private def constructConnections(g: Graph[Int, UnDiEdge]): Vector[Connection] = {
    val edges = GraphCommons.outerEdges(g)
    val zipped = edges.view.zipWithIndex.force
    val result = for (edge <- zipped) yield new Connection(edge._2, edge._1._1, edge._1._2)
    result.toVector
  }

}
