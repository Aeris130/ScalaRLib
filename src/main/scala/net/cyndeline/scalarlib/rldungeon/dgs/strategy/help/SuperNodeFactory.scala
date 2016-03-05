package net.cyndeline.scalarlib.rldungeon.dgs.strategy.help

import net.cyndeline.rlcommon.util.UnorderedPair
import net.cyndeline.rlgraph.biconnectivity.BiconnectedComponentsOperation
import net.cyndeline.rlgraph.biconnectivity.components.DFSComponentSearch
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.Room

import scala.Predef._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

/**
 * Detects biconnected components in a graph, and collapses them into a graph where each node either represents a single
 * vertex in the original graph, or multiple vertices belonging to the same biconnected component.
 *
 * An exception is made if 2 or more biconnected components with more than 1 edge share the same vertex as articulation
 * point. In this case, the point will be a member of both components collapsed node. It will also be given
 * its own node, and undirected dummy edges will be added between it and all its components.
 *
 * On directed edges: If two components are joined by a single directed edge, a directed collapsed edge will be added
 * between them in the resulting collapsed graph. If instead the nodes are connected by directed edges in both
 * directions, the collapsed graph will use an undirected edge instead since it's purpose is to provide a loop-free
 * data structure.
 *
 * @constructor Constructs a new factory for collapsing graphs into super-nodes.
 */
class SuperNodeFactory extends SuperNodeFactoryInterface {

  override def collapseCycles[R <: Room, C[X] <: EdgeLikeIn[X]](graph: Graph[R, C]): Graph[CollapsedNode, CollapsedEdge] = {
    val graphAndDirectedEdges = idGraph(graph)
    val graphWithIds = graphAndDirectedEdges._1 // Undirected
    val directedPairMap: Map[Int, Set[Int]] = graphAndDirectedEdges._2 // id's of vertices in directed edges
    val directedIdPairs = computeDirectedPairs(directedPairMap)
    val mergedDirectedEdges = new mutable.HashSet[UnorderedPair[Int]]()

    val componentFinder: BiconnectedComponentsOperation[Int, UnDiEdge] = new DFSComponentSearch[Int, UnDiEdge]()
    val bicomAndArtPoints = componentFinder.componentsAndArticulationPoints(graphWithIds)
    val bottleneckComponents: Vector[UnDiEdge[Int]] = bicomAndArtPoints.map(_._1).filter(_.edges.size == 1).map(_.edges.head.toOuter)
    val multiEdgeComponents = bicomAndArtPoints
      .filter(_._1.edges.size > 1)
      .map(entry => new MultiComponent(GraphCommons.outerVertices(entry._1).toSet, entry._2))

    // Special case if the graph contains a single room
    if (bicomAndArtPoints.isEmpty && graph.nodes.nonEmpty) {
      return Graph[CollapsedNode, CollapsedEdge](new CollapsedNode(graph.nodes.head.rid))
    }

    // Before converting multi-edge components into collapsed nodes, map each articulation point to the components
    // sharing it, to prepare for a dummy edge between the point and its components. Also remove such points from their
    // original component
    val dummyEdges = createDummies(multiEdgeComponents)
    val dummyNodes: Map[Int, CollapsedNode] = dummyEdges.map(_._1).distinct.map(r => r -> new CollapsedNode(r).asDummy).toMap

    val roomToSuperNodeAndComponent = mapRoomToComponent(multiEdgeComponents)
    val componentToNode = roomToSuperNodeAndComponent.values.map(kv => kv._2 -> kv._1).toMap
    val roomToSuperNode = roomToSuperNodeAndComponent.map(kv => kv._1 -> kv._2._1)
    val roomToSingleNode = createSingleNodes(bottleneckComponents, roomToSuperNode, dummyNodes) ++ dummyNodes

    var finalGraph = Graph[CollapsedNode, CollapsedEdge]()

    for (room <- roomToSuperNode.values.toVector ++ roomToSingleNode.values.toVector)
      finalGraph += room

    for (edge <- bottleneckComponents) {
      val from = dummyNodes.getOrElse(edge._1, roomToSuperNode.getOrElse(edge._1, roomToSingleNode(edge._1)))
      val to = dummyNodes.getOrElse(edge._2, roomToSuperNode.getOrElse(edge._2, roomToSingleNode(edge._2)))
      val ce = connectNodes(from, to, graphWithIds, directedIdPairs, mergedDirectedEdges)

      if (ce.nonEmpty)
        finalGraph += ce.get
    }

    for (edge <- dummyEdges) {
      val dummyNode = dummyNodes(edge._1)
      finalGraph += CollapsedEdge.dummyEdge(dummyNode, componentToNode(edge._2))
    }

    finalGraph
  }

  // Doesn't connect dummy edges
  private def connectNodes(a: CollapsedNode, b: CollapsedNode,
                           graph: Graph[Int, UnDiEdge],
                           directedEdges: Set[(Int, Int)],
                           mergedEdges: mutable.HashSet[UnorderedPair[Int]]): Option[CollapsedEdge[CollapsedNode]] = {
    val originalEdge = findOriginalEdge(a, b, graph)

    // The side of the edge that the node appears on must be preserved
    val aNode = if (a.contains(originalEdge._1)) a else b
    val bNode = if (aNode == a) b else a
    val originalFrom = if (aNode.isSuperNode) Some(originalEdge._1) else None
    val originalTo = if (bNode.isSuperNode) Some(originalEdge._2) else None
    val originalFromId = if (originalFrom.nonEmpty) originalFrom.get else originalEdge._1
    val originalToId = if (originalTo.nonEmpty) originalTo.get else originalEdge._2

    /* The edge is undirected, add a collapsed edge as usual. */
    if (!directedEdges.contains((originalFromId, originalToId)) && !directedEdges.contains((originalToId, originalFromId))) {
      Some(CollapsedEdge(aNode, bNode, originalFrom, originalTo))

    } else if (!directedEdges.contains((originalFromId, originalToId)) || !directedEdges.contains((originalToId, originalFromId))) {
      /* Edge is directed, but no directed edge exists in the opposite direction. */
      val existingPair = directedEdges.find(p => p == (originalFromId, originalToId) || p == (originalToId, originalFromId)).get

      if (existingPair._1 == originalFromId)
        Some(DiCollapsedEdge(aNode, bNode, originalFrom, originalTo))
      else
        Some(DiCollapsedEdge(bNode, aNode, originalTo, originalFrom))

    } else if (!mergedEdges.contains(UnorderedPair(originalFromId, originalToId))) {
      /* Edge is directed and an opposite directed edge exists as well, and a collapsed merged edge based on the
       * opposite edge has not been added already.
       */
      mergedEdges += UnorderedPair(originalFromId, originalToId)
      Some(CollapsedEdge(aNode, bNode, originalFrom, originalTo, true))

    } else {
      /* No edge needs to be added since one was already created based on the opposite edge. */
      None
    }

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

  // Computes edges between a cutpoint and its multi-edge components.
  private def createDummies(multiNodeComponents: Vector[MultiComponent]): Vector[(Int, MultiComponent)] = {
    val componentsPerPoint = new mutable.HashMap[Int, mutable.HashSet[MultiComponent]]()

    // Map every cutpoint between multi-edge components to the components containing it
    for (c <- multiNodeComponents; ap <- c.articulationPoints) {
      val currentComponents = componentsPerPoint.getOrElse(ap, {
        val s = mutable.HashSet[MultiComponent]()
        componentsPerPoint put(ap, s)
        s
      })
      currentComponents += c
    }

    for (cpp <- componentsPerPoint.toVector.filter(_._2.size > 1); component <- cpp._2) yield (cpp._1, component)
  }

  /**
    * @return Every pair of vertices that contains directed edges in both direction. Example: If vertices 1 and 2 has
    *         edges 1~>2 and 2~>1, then (1,2) and (2,1) will be in this set.
    */
  private def computeDirectedPairs(directedPairs: Map[Int, Set[Int]]): Set[(Int, Int)] = {
    var edges = Set[(Int, Int)]()
    for (keyValue <- directedPairs; neighbor <- keyValue._2) {
      edges += ((keyValue._1, neighbor))
    }

    edges
  }

  /**
    * Note: This method computes an undirected graph, thus every multi-edge set will be replaced by a single
    * undirected edge.
    *
    * @return An undirected graph, as well as mappings of all directed vertex pairs in the graph represented by
    *         their id's.
    */
  private def idGraph[R <: Room, C[X] <: EdgeLikeIn[X]](graph: Graph[R, C]): (Graph[Int, UnDiEdge], Map[Int, Set[Int]]) = {
    val directedPairs = new mutable.HashMap[Int, Set[Int]]()
    val nodes = GraphCommons.outerVertices(graph)
    val edges = graph.edges.map(_.toOuter).toVector
    var g = Graph[Int, UnDiEdge]()

    for (n <- nodes)
      g += n.rid

    for (e <- edges) {
      if (e.isDirected) {
        val from = e._1.rid
        val to = e._2.rid
        val currentEdges = directedPairs.getOrElse(from, Set[Int]())
        directedPairs.put(from, currentEdges + to)
      }

      g += e._1.rid ~ e._2.rid
    }

    (g, directedPairs.toMap)
  }

  private class MultiComponent(vs: Set[Int], val articulationPoints: Set[Int]) {
    var rooms: Set[Int] = vs
    var cutPoints = articulationPoints
  }

}
