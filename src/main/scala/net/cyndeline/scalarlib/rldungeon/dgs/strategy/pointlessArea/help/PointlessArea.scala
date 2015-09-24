package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}

import scala.collection.mutable.ListBuffer
import scalax.collection.immutable.Graph

/**
 * A set of nodes and edges that can be reached from each other without traversing a node on the main path.
 *
 * @param topology Every node and edge in the pointless area. Contains the main path connection and the edge
 *                 that connects it.
 * @param mainPathConnection The only node on the main path that can be reached from this area.
 */
case class PointlessArea(topology: Graph[CollapsedNode, CollapsedEdge], mainPathConnection: CollapsedNode) {
  require(topology.isConnected, "The pointless area must be connected")
  require(topology.contains(mainPathConnection), "The pointless area must contain the main path connection")
  require(!(topology - mainPathConnection).isEmpty, "The pointless area must contain more than the main path connection.")

  /**
   * Removes a path from the main connection to a target from this area. Doing so creates a number of new areas.
   * @param path The path in the topology going from the main path connection to another node that should be
   *             removed from the area.
   * @return Every new pointless area that is created by removing every node and edge from the main connection to
   *         the target. Empty if the pointless area only contains the path.
   */
  def split(path: Path[CollapsedNode, CollapsedEdge]): Vector[PointlessArea] = {

    val nodesOnPath = path.vertices.toSet
    val pointlessAreas = new ListBuffer[PointlessArea]()

    /* It's possible that multiple sub-paths will be connected to the same node along the path, so it's not enough
     * to simply delete edges and compute connected components. */
    for (n <- path.vertices) {
      val neighborsInPointlessSubGraphs = GraphCommons.outerNeighbors(n, topology).filterNot(nodesOnPath.contains)

      for (pn <- neighborsInPointlessSubGraphs) {
        val edge = topology.get(n).edges.find(_.contains(pn)).get
        val subGraph = GraphCommons.computeSubTopology[CollapsedNode, CollapsedEdge](edge.toOuter, n, topology)
        pointlessAreas += PointlessArea(subGraph, n)
      }
    }

    pointlessAreas.toVector
  }

}
