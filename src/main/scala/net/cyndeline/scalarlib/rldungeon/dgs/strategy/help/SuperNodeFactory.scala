package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import net.cyndeline.rlgraph.biconnectivity.BiconnectedComponentsOperation
import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.Room

import scala.Predef._
import scala.collection.mutable
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Detects biconnected components in a graph, and collapses them into a graph where each node either represents a single
 * vertex in the original graph, or multiple vertices belonging to the same biconnected component.
 *
 * An exception is made if 2 or more biconnected components with more than 1 edge share the same vertex as articulation
 * point. In this case, the point will not be a member of either components collapsed node. Instead it will be given
 * its own node, and dummy edges will be added between it and all its components. The reason that the point will not
 * also be present in the original nodes is that doing so might cause the point to be submitted as an activator
 * candidate for multiple areas, giving the player no incentive to enter them and instead turn around after visiting
 * the cutpoint.
 *
 * @constructor Constructs a new factory for collapsing graphs into super-nodes.
 */
class SuperNodeFactory extends SuperNodeFactoryInterface {

  override def collapseCycles[R <: Room, C[X] <: UnDiEdge[X]](graph: Graph[R, C]): Graph[CollapsedNode, CollapsedEdge] = {
    val graphWithIds = idGraph(graph)
    val componentFinder: BiconnectedComponentsOperation[Int, UnDiEdge] = new DFSComponentSearch[Int, UnDiEdge]()
    val bicomAndArtPoints = componentFinder.componentsAndArticulationPoints(graphWithIds)
    val bottleneckComponents: Vector[UnDiEdge[Int]] = bicomAndArtPoints.map(_._1).filter(_.edges.size == 1).map(_.edges.head.toOuter)
    val multiEdgeComponents = bicomAndArtPoints
      .filter(_._1.edges.size > 1)
      .map(entry => new MultiComponent(GraphCommons.outerVertices(entry._1).toSet, entry._2))

    // Special case if the graph contains a single room
    if (bicomAndArtPoints.isEmpty && !graph.nodes.isEmpty) {
      return Graph[CollapsedNode, CollapsedEdge](new CollapsedNode(graph.nodes.head.rid))
    }

    // Before converting multi-edge components into collapsed nodes, map each articulation point to the components
    // sharing it, to prepare for a dummy edge between the point and its components. Also remove such points from their
    // original component
    val dummyEdges = createDummies(multiEdgeComponents)
    val dummyNodes = dummyEdges.map(_._1).distinct.map(r => r -> new CollapsedNode(r).asDummy).toMap

    val roomToSuperNodeAndComponent = mapRoomToComponent(multiEdgeComponents)
    val componentToNode = roomToSuperNodeAndComponent.map(_._2).map(kv => kv._2 -> kv._1)
    val roomToSuperNode = roomToSuperNodeAndComponent.map(kv => kv._1 -> kv._2._1)
    val roomToSingleNode = createSingleNodes(bottleneckComponents, roomToSuperNode, dummyNodes) ++ dummyNodes

    var finalGraph = Graph[CollapsedNode, CollapsedEdge]()

    for (room <- roomToSuperNode.values.toVector ++ roomToSingleNode.values.toVector)
      finalGraph += room

    for (edge <- bottleneckComponents) {
      val from = roomToSuperNode.getOrElse(edge._1, roomToSingleNode(edge._1))
      val to = roomToSuperNode.getOrElse(edge._2, roomToSingleNode(edge._2))
      val ce = connectNodes(from, to, graphWithIds)
      finalGraph += ce
    }

    for (edge <- dummyEdges) {
      val dummyNode = dummyNodes(edge._1)
      finalGraph += CollapsedEdge.dummyEdge(dummyNode, componentToNode(edge._2))
    }

    finalGraph
  }

  // Doesn't connect dummy edges
  private def connectNodes(a: CollapsedNode, b: CollapsedNode, graph: Graph[Int, UnDiEdge]): CollapsedEdge[CollapsedNode] = {
    val originalEdge = findOriginalEdge(a, b, graph)

    // The side of the edge that the node appears on must be preserved
    val aNode = if (a.contains(originalEdge._1)) a else b
    val bNode = if (aNode == a) b else a
    val originalFrom = if (aNode.isSuperNode) Some(originalEdge._1) else None
    val originalTo = if (bNode.isSuperNode) Some(originalEdge._2) else None
    CollapsedEdge(aNode, bNode, originalFrom, originalTo)
  }

  private def findOriginalEdge(a: CollapsedNode, b: CollapsedNode, graph: Graph[Int, UnDiEdge]): UnDiEdge[Int] = {
    val from = a.vertexCollection.find(r => graph.get(r).neighbors.exists(n => b.contains(n))).get
    val to = b.vertexCollection.find(r => graph.get(r).edges.exists(_.contains(from))).get
    graph.get(from).edges.find(_.contains(to)).get.toOuter
  }

  /* Maps every room that is a member of a super-node to the set of rooms in the node. */
  private def mapRoomToComponent(multiEdgeComponents: Vector[MultiComponent]): Map[Int, (CollapsedNode, MultiComponent)] = {
    val r: Vector[(Int, (CollapsedNode, MultiComponent))] = for {
      component <- multiEdgeComponents
      superNode = new CollapsedNode(component.rooms)
      room <- component.rooms
    } yield (room, (superNode, component))

    r.toMap // This works since common cutpoints in each component should be removed by now.
  }

  private def createSingleNodes(bottlenecks: Vector[UnDiEdge[Int]],
                                multiNodes: Map[Int, CollapsedNode],
                                dummyNodes: Map[Int, CollapsedNode]): Map[Int, CollapsedNode] = {
    val singleRooms: Vector[Int] = bottlenecks
      .map(e => Vector(e._1, e._2)).flatten
      .filter(r => !multiNodes.contains(r) && !dummyNodes.contains(r))

    singleRooms.map(r => r -> new CollapsedNode(r)).toMap
  }

  // Computes edges between a cutpoint and its multi-edge components. Also removes the cutpoint from them.
  private def createDummies(multiNodeComponents: Vector[MultiComponent]): Vector[(Int, MultiComponent)] = {
    val componentsPerPoint = new mutable.HashMap[Int, mutable.HashSet[MultiComponent]]()

    // Map every cutpoint between multi-edge components to the components containing it
    for (c <- multiNodeComponents; ap <- c.articulationPoints) {
      val currentComponents = componentsPerPoint.get(ap).getOrElse {
        val s = mutable.HashSet[MultiComponent]()
        componentsPerPoint put (ap, s)
        s
      }
      currentComponents += c
    }

    // We don't care about all cutpoints, just the ones shared between multi-edge components. Any cutpoint that
    // doesn't have 2+ such components can be ignored.
    for (c <- multiNodeComponents; p <- c.cutPoints) {
      if (componentsPerPoint(p).size < 2)
        c.removePoint(p)
      else
        c.removeRoom(p)
    }

    for (cpp <- componentsPerPoint.toVector.filter(_._2.size > 1); component <- cpp._2) yield (cpp._1, component)
  }

  private def idGraph[R <: Room, C[X] <: UnDiEdge[X]](graph: Graph[R, C]): Graph[Int, UnDiEdge] = {
    val nodes = GraphCommons.outerVertices(graph)
    val edges = graph.edges.map(_.toOuter).toVector
    var g = Graph[Int, UnDiEdge]()

    for (n <- nodes)
      g += n.rid

    for (e <- edges)
      g += e._1.rid ~ e._2.rid

    g
  }

  private class MultiComponent(vs: Set[Int], val articulationPoints: Set[Int]) {
    var rooms: Set[Int] = vs
    var cutPoints = articulationPoints

    def removeRoom(r: Int) { rooms -= r }
    def removePoint(p: Int) { cutPoints -= p }

  }

}
