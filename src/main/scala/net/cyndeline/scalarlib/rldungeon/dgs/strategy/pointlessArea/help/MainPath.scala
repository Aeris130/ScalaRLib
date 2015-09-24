package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rlgraph.pathfinding.{BFSPathfinder, Path}
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.algorithms.CollapsedGraphEdgeTrimmer

/**
 * Used when assigning activators and responders. This object represents the current path through the level that
 * the player must take to collect every activator. As Davis Adams notes in his article "Automatic Generation of
 * Dungeons for Computer Games", there is no need to represent multiple ways of traversing the level, as visiting
 * every activator will cause the same rooms and corridors to be visited no matter which order you choose.
 *
 * This class computes pointless areas (any area not on the main path) every time it is instantiated or modified.
 * Modifications can be made by taking a subset of a pointless area and appending it to the main path.
 *
 * @param currentPath The current main path through a level.
 * @param pointlessAreas All pointless areas connected to the current main path. Whenever an area is split to multiple
 *                       areas, the splits are appended at the back of the list. Thus the head element has been in
 *                       the list the longest.
 */
class MainPath private (val currentPath: Path[CollapsedNode, CollapsedEdge],
                        val pointlessAreas: Vector[PointlessArea]) {

  /**
   * @param target A node present in one of the main paths pointless areas.
   * @param area The area containing the node.
   * @return A new main path, appended with w path going from it and to the target node, then back again to the main
   *         area connection of the pointless area.
   */
  def appendPath(target: CollapsedNode, area: PointlessArea): MainPath = {
    require(pointlessAreas.contains(area), "No pointless area object was found, cannot append path.")
    require(target != area.mainPathConnection, "The main path connection was selected as a target when splitting pointless areas.")
    require(area.topology.contains(target), "The selected target is not a member of the pointless area.")
    val path = BFSPathfinder().computePath(area.mainPathConnection, target, area.topology).get
    val remainingAreas = pointlessAreas diff List(area)
    val resultingPointlessAreas = area.split(path)

    /* The nodes that should be appended to the current main path are a loop going from the main area connection to the
     * target, then back to the connection again. Since the connection is already a part of the current path, only the
     * second connection instance is added.
     */
    val loopToAppend = loopingPath(path)

    val nodeSplit = splitCurrentPathElement(area.mainPathConnection, currentPath.vertices)
    val edgeSplit = if (area.mainPathConnection == currentPath.start) {
      (Vector(), currentPath.edges)
    } else if (area.mainPathConnection == currentPath.stop) {
      (currentPath.edges, Vector())
    } else {
      val edgeBeforeAppend = currentPath.edges.find(_.contains(area.mainPathConnection)).get
      splitCurrentPathElement(edgeBeforeAppend, currentPath.edges)
    }

    val finalNodes = nodeSplit._1 ++ loopToAppend.vertices ++ nodeSplit._2
    val finalEdges = edgeSplit._1 ++ loopToAppend.edges ++ edgeSplit._2
    val appendedPath = Path(finalNodes.head, finalEdges)

    new MainPath(appendedPath, remainingAreas ++ resultingPointlessAreas)
  }

  /**
   * Removes a node from a pointless area, and trims any new leaf nodes that results from doing that until a node
   * with degree > 2 is found.
   * @param target Node to remove.
   * @param area Pointless area to remove node from.
   * @return A copy of this main path with the pointless area trimmed. If doing so causes the area to become empty
   *         (i.e it only contains the main connection), it is not included. Otherwise it is re-appended at the start
   *         of the area vector.
   */
  def removeNode(target: CollapsedNode, area: PointlessArea): MainPath = {
    val evaluator = (n: CollapsedNode) => false
    val trimmedTopology = new CollapsedGraphEdgeTrimmer().trimSingleBranch(area.topology, target, Set(area.mainPathConnection), evaluator)
    val remainingAreas = pointlessAreas diff List(area)

    val newPointlessAreas = if (trimmedTopology.nodes.size == 1) { // The only remaining node is the main area connection
      remainingAreas
    } else {
      new PointlessArea(trimmedTopology, area.mainPathConnection) +: remainingAreas
    }

    new MainPath(currentPath, newPointlessAreas)
  }

  /**
   * @param p A path from a main connection to a pointless node.
   * @return A new path that doesn't contain the first node in the original path (the main connection), that loops back
   *         and ends in the main connection again.
   */
  private def loopingPath(p: Path[CollapsedNode, CollapsedEdge]): Path[CollapsedNode, CollapsedEdge] = {
    val nodes = p.vertices.drop(1)
    val edges = p.edges
    val loopingEdges = edges ++ p.edges.reverse
    Path(nodes.head, loopingEdges)
  }

  /**
   * Splits the current path at the first instance of a node. The first instance can be used since only loops are added,
   * so it doesn't matter where they are inserted.
   * @param splitAt Node to split at.
   * @return Two vectors: Once containing every node up until (and including) the split node, the second contains the
   *         rest.
   */
  private def splitCurrentPathElement[E](splitAt: E, es: Vector[E]): (Vector[E], Vector[E]) = {
    val iterator = es.iterator
    var i = 0
    while (iterator.hasNext) {
      val n = iterator.next()
      if (n == splitAt) {
        return es.splitAt(i + 1)
      }

      i += 1
    }

    throw new Error("Could not find the target element to split the current main path at.")
  }

}

/**
 * Constructs an initial main path.
 */
object MainPath {

  /**
   * @param graph A collapsed graph representing an entire level.
   * @param start The collapsed node containing the id of the start room.
   * @param goal The collapsed node containing the id of the goal room (may be the same as start).
   * @return Returns an initial non-looping main path with every pointless area not visited when traversing from
   *         start to goal.
   */
  def apply(graph: Graph[CollapsedNode, CollapsedEdge], start: CollapsedNode, goal: CollapsedNode): MainPath = {
    val mainPath = BFSPathfinder().computePath(start, goal, graph)
      .getOrElse(throw new Error("No path from start to goal was found, cannot compute pointless areas."))

    /* Here we can make use of the pointless area class, and consider the entire graph to be pointless. Then split it
     * along the initial main path to compute the initial set of pointless areas.
     */
   val pointlessAreas = new PointlessArea(graph, start).split(mainPath)
   new MainPath(mainPath, pointlessAreas)
  }
}
