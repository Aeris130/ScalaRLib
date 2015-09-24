package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.algorithms

import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData
import net.cyndeline.scalarlib.rldungeon.common.Room
import net.cyndeline.scalarlib.rlgraph.pathfinding.Path

/**
 * For dependency injection.
 */
trait EdgeTrimmer {
  def removeVertices(graph: Graph[CollapsedNode, CollapsedEdge],
                     mainPath: Set[CollapsedNode],
                     evaluator: CollapsedNode => Boolean): Graph[CollapsedNode, CollapsedEdge]

  def trimSingleBranch(graph: Graph[CollapsedNode, CollapsedEdge],
                       start: CollapsedNode,
                       mainPath: Set[CollapsedNode],
                       evaluator: CollapsedNode => Boolean): Graph[CollapsedNode, CollapsedEdge]
}

/**
 * Removes the outermost nodes on each branch of a tree until every leaf node in the tree is accepted by a
 * user-specified method.
 *
 * @constructor Creates a new edge trimmer.
 */
class CollapsedGraphEdgeTrimmer extends EdgeTrimmer {

  /**
   * Repeatedly finds a node with degree 1 that fails the users evaluator. When such a node is found, the edge is
   * traversed inwards (repeatedly removing nodes) until a node that satisfies one of the following criterias is found:
   *
   *  1. The node belongs to the main path
   *  1. The node has degree > 2
   *  1. The node is accepted by the evaluator
   *
   * @param graph Graph to trim edges of.
   * @param mainPath Path containing nodes that shouldn't be removed.
   * @param evaluator Returns true if a node should be kept, otherwise false. This is used to allow the trimmer
   *                  to evaluate collapsed graphs with different criteria (i.e to keep nodes that can hold rewards,
   *                  or to keep those that can hold activators.
   * @return A graph where every edge-node (degree 1) is accepted by the evaluator.
   */
  override def removeVertices(graph: Graph[CollapsedNode, CollapsedEdge],
                              mainPath: Set[CollapsedNode],
                              evaluator: CollapsedNode => Boolean): Graph[CollapsedNode, CollapsedEdge] = {
    var currentGraph = graph
    var rejectedNode: Option[CollapsedNode] = findRejectedEdgeNode(currentGraph, mainPath, evaluator)

    while (rejectedNode.isDefined) {
      val n = rejectedNode.get
      currentGraph = trimSingleBranch(currentGraph, n, mainPath, evaluator)
      rejectedNode = findRejectedEdgeNode(currentGraph, mainPath, evaluator)
    }

    currentGraph
  }

  /**
   * Starts at a leafnode in a graph and removes nodes inwards until a node is found that is accepted by the
   * evaluator, or that has degree > 2.
   * @param graph Graph to trim.
   * @param start Leaf node that fails the evaluator.
   * @param mainPath Set of nodes that the algorithm should stop at if it reaches them.
   * @param evaluator Returns true if a node should be kept, otherwise false.
   * @return The trimmed graph.
   */
  override def trimSingleBranch(graph: Graph[CollapsedNode, CollapsedEdge],
                               start: CollapsedNode,
                               mainPath: Set[CollapsedNode],
                               evaluator: CollapsedNode => Boolean): Graph[CollapsedNode, CollapsedEdge] = {
    require(!evaluator(start), "Cannot trim a branch if the start node doesn't fail the evaluator.")
    require(graph.get(start).degree == 1, "Cannot trim a branch unless it starts at a node with degree 1.")
    var currentGraph = graph
    val nodes = findNodesToRemove(start, graph, mainPath, evaluator)
    val it = nodes.iterator
    while (it.hasNext) {
      val nodeToRemove = it.next()
      currentGraph -= nodeToRemove
    }
    currentGraph
  }

  /**
   * Finds a node at the edge of a branch that is rejected by the evaluator.
   * @param graph Graph with the node set to search.
   * @return A pointless node of degree 1 that is rejected by the evaluator, or None if no such node exists.
   */
  private def findRejectedEdgeNode(graph: Graph[CollapsedNode, CollapsedEdge],
                                  mainNodes: Set[CollapsedNode],
                                  evaluator: CollapsedNode => Boolean): Option[CollapsedNode] = {
    val inner = graph.nodes.find(n => {
      if (!mainNodes.contains(n) && n.degree <= 1) {
        !evaluator(n)
      } else {
        false
      }
    })

    if (inner.isDefined) {
      val outer: CollapsedNode = inner.get
      Option(outer)
    } else {
      None
    }
  }

  /**
   * Follows a path inwards from an edge node until a node that is either accepted, or is in the center of an
   * intersection that may lead to other pointless nodes is found.
   *
   * This has to be done in a method of its own since the scala compiler complains about type incompatibility
   * when traversing in a method call having multiple graph instances of the same type.
   *
   * @param node Node to start traversing from.
   * @param graph Graph to traverse.
   * @return All traversed nodes that doesn't match the above properties that can be reached from the initial node
   *         without passing a node with matching properties.
   */
  private def findNodesToRemove(node: CollapsedNode,
                                graph: Graph[CollapsedNode, CollapsedEdge],
                                mainNodes: Set[CollapsedNode],
                                evaluator: CollapsedNode => Boolean): Set[CollapsedNode] = {
    val inner = graph.get(node)
    var nodes = Set[CollapsedNode]()

    def addToNodes(n: graph.NodeT) {
      nodes += n
    }

    inner.withSubgraph(n => n.degree <= 2 && !mainNodes.contains(n) && !evaluator(n)).foreach(addToNodes)
    nodes
  }

}
