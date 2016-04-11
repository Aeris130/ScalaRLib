package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.rlgraph.pathfinding.BFSPathfinder
import net.cyndeline.rlgraph.traversal.BFSTraversal
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.Room
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.higherKinds
import scala.tools.nsc.interpreter.Completion.Candidates
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Computes all sets of edges that lie on the main path such that they can receive a responder without blocking
 * their own activator. Three cases to consider:
 *
 *  1. The responder may be placed on any edge that appears along the current main path after the main area connection
 *  belonging to the activator has been traversed once.
 *  1. If the main area connection does not belong to the initial main path, edges between the connection and
 *  the initial main path cannot be used as candidates as it would cause the responder to block off the activator.
 *  1. If the main path contains a directed edge, with no directed edge going in the opposite direction, only the edges
 *  up until and including the directed edge may be considered, as the player might not otherwise be able backtrack in
 *  order to find the activator.
 */
class EdgeSelection {

  /**
    * Finds every edge that can carry a responder to an activator without risking that the activator is blocked off.
    *
    * A note on which edge candidates are returned: If the main path loops over the main connection twice or more,
    * only the edges that lie between the first and last occurrences will be returned. The returned result
    * contains multiple vectors, where each vector either contains every edge candidate (only one vector will
    * be present if this is the case, since the area connection only occurred once), or every candidate between
    * one occurrence of the connection to the next. Regardless, the closer to the start of each vector a
    * candidate lies, the closer it is to the area connection, and subsequently the activator that matches
    * the responder about to be replaced.
    *
    * @param vertices Vertices on the main path. If a vertex is visited more than once, there will be duplicates.
    * @param edges Edges on the main path. If an edge is visited more than once, there will be duplicates.
    * @param areaConnection A vertex on the main path that connects a pointless area that is about to receive an
    *                       activator.
    * @param initialAreaConnections The set of collapsed nodes that made up the initial main path. Any area connection
    *                               that isn't a member of this set has a path to one of the nodes in it.
    * @return Every edge that is a valid candidate (topology-wise) for the responder matching the activator. See
    *         method description for more info.
    */
  def findEdgeCandidates(vertices: Vector[CollapsedNode],
                         edges: Vector[CollapsedEdge[CollapsedNode]],
                         areaConnection: CollapsedNode,
                         initialAreaConnections: Set[CollapsedNode]): Vector[EdgeCandidate] = {
    require(vertices.nonEmpty, "Cannot compute edge candidates without nodes on the main path.")
    if (vertices.drop(1).isEmpty || areaConnection == vertices.last)
      return Vector()

    /* Every edge on the main path that comes after the area connection has been found. Since the connection will have
     * an edge coming into it before it is traversed, that edge is dropped. The exception is if the area connection is
     * the first node on the main path, in which case no edge needs to be dropped.
     */
    val mainPathCandidates = if (areaConnection == vertices.head) edges else edges.span(!_.contains(areaConnection))._2.drop(1)

    /* The graph needs to only contains potential candidates since otherwise edges prior to the connection will be
     * found using the BFS.
     */
    val mainPathGraph = Graph.from(Nil, mainPathCandidates)
    val candidates = new ArrayBuffer[EdgeCandidate]()
    BFSTraversal(mainPathGraph, areaConnection, 0, candidateFactory(candidates) _) // Candidates are added by the visitor

    // Edges that would block off the activators pointless area if they received a responder
    val ineligibleEdges: Set[CollapsedEdge[CollapsedNode]] = if (!initialAreaConnections.contains(areaConnection)) {
      val pathToMain = (mainPathGraph get areaConnection).pathUntil(n => initialAreaConnections.contains(n))
        .getOrElse(throw new Error("No path to the main path found for node " + areaConnection))

      pathToMain.edges.map(_.toOuter).toSet
    } else {
      Set()
    }

    val eligibleEdgeCandidates = candidates.toVector.filterNot(c => ineligibleEdges.contains(c.edge)).filterNot(_.edge.isDummy)
    val cutoffAtFirstDirectedEdge = eligibleEdgeCandidates.span(!_.edge.isDirected)

    if (cutoffAtFirstDirectedEdge._2.isEmpty)
      eligibleEdgeCandidates
    else
      cutoffAtFirstDirectedEdge._1 :+ cutoffAtFirstDirectedEdge._2.head
  }

  // Sent as visitor to the BFS traversal
  private def candidateFactory(candiates: ArrayBuffer[EdgeCandidate])(from: CollapsedNode, to: CollapsedNode, edge: CollapsedEdge[CollapsedNode]): Unit = {
    candiates += new EdgeCandidate(edge, from)
  }

  /**
    * This wrapper is needed in case the selected edge is undirected and represents a pair of directed edges. If so, it
    * will select the directed edge that is traversed in the path from start to goal, and not the opposite.
    *
    * @param c A collapsed edge that can carry a responder.
    * @param from The first collapsed node that is encountered when traversing the main path from start to goal.
    */
  class EdgeCandidate(c: CollapsedEdge[CollapsedNode], val from: CollapsedNode) {
    require(c.contains(from), "The collapsed node " + from + " was not a member of the edge " + c)
    require(!c.isDirected || c._1 == from, "The edge " + c + " was directed, but its specified start node " + from + " was not the same as its start node " + c._1)

    def edge: CollapsedEdge[CollapsedNode] = c

    def isDirected: Boolean = c.isDirected

    /**
      * @return The original edge candidate selected to receive a responder.
      */
    def retrieveOriginalEdge[R <: Room, E[X] <: UnDiEdge[X]](level: Graph[R, E], roomIdMap: Map[Int, R]): E[R] = {
      val originalTargetsOfEdge = CollapsedEdge.targetsOfEdge(c)
      val originalFrom = roomIdMap(originalTargetsOfEdge._1)
      val originalTo = roomIdMap(originalTargetsOfEdge._2)

      GraphCommons.outerEdgeNeighbors(originalFrom, level).find(_._1 == originalTo).get._2
    }

  }
}


