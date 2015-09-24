package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}

import scala.collection.mutable.ListBuffer

/**
 * Computes all sets of edges that lie on the main path such that they can receive a responder without blocking
 * their own activator. Two cases to consider:
 *
 *  1. If the main area connection of the area receiving the activator only occurs once on the main path, every
 *  edge after the connection may receive the responder.
 *  1. If the connection occurs twice or more, only the edges present after the connection has been visited once, and
 *  before it is visited for the last time may be considered.
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
   * @param vertices Vertices on the main path.
   * @param edges Edges on the main path.
   * @param areaConnection A vertex on the main path that connects a pointless area that is about to receive an
   *                       activator.
   * @return Every edge that is a valid candidate (topology-wise) for the responder matching the activator. See
   *         method description for more info.
   */
  def findEdgeCandidates(vertices: Vector[CollapsedNode],
                         edges: Vector[CollapsedEdge[CollapsedNode]],
                         areaConnection: CollapsedNode): Vector[Vector[CollapsedEdge[CollapsedNode]]] = {
    val candidates = new ListBuffer[Vector[CollapsedEdge[CollapsedNode]]]()
    var i = 0
    val lastEdge = edges.size
    var add = false
    var connectionVisits = 0
    val currentEdgeList = new ListBuffer[CollapsedEdge[CollapsedNode]]()

    while (i < lastEdge) {
      val node = vertices(i)
      val edge = edges(i)

      if (node == areaConnection) {
        add = true
        connectionVisits += 1
        if (!currentEdgeList.isEmpty) { // This will happen if the connection is traversed multiple times
          candidates += currentEdgeList.toVector
          currentEdgeList.clear()
        }
      }

      if (add) {
        currentEdgeList += edge
      }
      i += 1
    }

    /* Only add the edges from the last occurrence of the main connection if a single occurrence was found. */
    if (connectionVisits == 1) {
      candidates += currentEdgeList.toVector
    }

    /* Distinct removes duplicates due to loops, 1, 2, 3, 4, 3, 2, 1 -> 1, 2, 3, 4. Since .distinct preserves the
     * elements that appears first when removing duplicates, every edge is still present in the order they're visited
     * when traversing the main path.
     */
    candidates.toVector.map(_.distinct).map(edges => edges.filterNot(_.isDummy)).filterNot(_.isEmpty)
  }

}
