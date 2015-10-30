package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea

import net.cyndeline.rlgraph.pathfinding.{BFSPathfinder, LongestTreePath, Path}
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.{Corridor, PointlessLevel, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode, SuperNodeFactory, SuperNodeFactoryInterface}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.algorithms.{CollapsedGraphEdgeTrimmer, EdgeTrimmer}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help.{ClosestEdgeFinder, EdgeSelection, MainPath}
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy
import net.cyndeline.scalarlib.rldungeon.levelPath.TreePath

import scala.language.reflectiveCalls
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * Adds activator- and responder pairs to a level in order to remove pointless areas by making the player diverge
 * from the main path before reaching the goal. Activators and responders are general representations for game-specific
 * objects that hinders player progression. Responders are present in connections between rooms in the level, and
 * signifies that the player should not be able to move from one room to the other until the activator that matches
 * the responder has been reached. How this is achieved in the actual game is up to the user, this strategy only tags
 * room/corridor pairs to mark them as recipients of a matching activator/responder (one common example in games would
 * be activators == keys, responders == locks).
 *
 * NOTE: This strategy guarantees that the level remains beatable even if every assigned A/R pair is placed on the
 * level (i.e there will never be a situation where the player cannot progress because an activator required to disable
 * a responder lies beyond the responder on the level). However, this is only the case if the level only has a single
 * start and goal. As such, the PointlessLevel trait only specifies one such pair, having the player enter the
 * level at any other room than the ones specified as start/goal may make the level unbeatable. It is possible to
 * make use of multiple exits, but be aware that the player might reach them (except the one specified by the trait)
 * without first clearing all A/R pairs.
 *
 * ==Algorithm outline==
 *
 * In order to guarantee that a responder between rooms A and B accurately blocks off all rooms beyond it, there cannot
 * be any other paths on the level from A to B (or vice versa) that is unblocked (if so the responder can be
 * circumvented). The runtime for finding all such paths has exponential runtime (first all paths from A to B must be
 * found and blocked by a responder, then all paths from one node on those paths to another node on the same path must
 * be found to ensure that ''those'' responders cannot be circumvented, then all paths between ''those'' paths must be
 * found etc.). As this may lead to unacceptable runtimes for large levels, this strategy employs a heuristic that works
 * by collapsing all biconnected components (sections of the level where every room has two or more paths to every other
 * room in the section) into nodes, and only placing responders on edges that connect these nodes in the resulting
 * graph-representation. As the collapsed graph is a tree, only one path between any two sections of the level can
 * exist.
 *
 * The algorithm works as follows:
 *
 *  1. Compute the initial main path from start to goal. Add every room not on the path to a pointless area.
 *  Any given pointless area contains all nodes and edges that can be reached from each other without passing a node
 *  on the main path.
 *  1. Find a node on the main path that is connected to a pointless area (this node is called the main [path]
 *  connection).
 *  1. Select a node inside the pointless area that can carry activators. Select an edge along the main path that
 *  occurs below (i.e closer to he goal) the current main connection and can hold a responder. Add the
 *  activator/responder pair to a room inside the node and the the corridor represented by the edge.
 *   1. Note that in order to guarantee that the level remains beatable, edges along the main path may only be
 *   considered until the main connection is traversed for the last time, if the main path visits the same node twice
 *   or more. See example below.
 *  1. Compute the path going from the main connection to the node selected for the activator and append it to the
 *  main path such that it loops from the main connection to the activator and back again.
 *   1. If a node is found but no edge below the main connection is capable of carrying responders, this will not
 *   change with the addition of future responders. Instead the node is added to the main path in this step anyway,
 *   ensuring that the entire graph eventually will be encompassed by it.
 *  1. Compute new pointless areas (if any exists) that results from making the nodes along the new main path
 *  meaningful.
 *  1. Repeat the algorithm using the new main path.
 *
 *  Example: A level with the initial main path (start) > 1-2-3-4 > (goal), and a pointless area connected to
 *  room 2: 2-5, 5-6, 5-7
 *
 *  The algorithm selects the pointless area (since there's only one in this example), and selects a node in it to
 *  receive the activator (node 6). By doing this, node 5 and 6 are no longer pointless, as they must both be traversed
 *  to get the activator. When selecting an edge for the responder, all edges below 2 (2-3, 3-4) are eligible. The main
 *  path is appended with 2-5-6-5-2 (as it loops from 2 to 6 and back), making it 1-2-5-6-5-2-3-4. Node 7 is now the
 *  only node in a pointless area, with the main path connection 5.
 *
 *  The area with node 7 is selected for another activator. When traversing the main path from node 5 towards the goal
 *  however (5>6>5>2>3...), only the edge 5-6 is visited before the path loops back to 5 again (the main connection)
 *  for the last time. To ensure a valid level, the responder can only be placed between 5 and 6. If an edge past the
 *  main connection (5) were considered, edge 2-5 might be selected and block off its own activator.
 */
class ActivatorResponderStrategy[L <: PointlessLevel[L, R, C], R <: Room : TypeTag, C[X] <: UnDiEdge[X] with Corridor: ({type l[M[_]] = TypeTag[M[R]]})#l] private
  (edgeTrimmer: EdgeTrimmer, superNodeFactory: SuperNodeFactoryInterface) extends Strategy[L, R, C] {

  /**
   * Constructs a new ActivatorResponder strategy.
   */
  def this() = this(new CollapsedGraphEdgeTrimmer, new SuperNodeFactory())

  def apply(level: L): Option[L] = {
    require(level.responderAmount >= 0, "Responder amount must be 0 or higher.")

    val collapsedGraph = superNodeFactory.collapseCycles(level.asGraph)
    val initialMainPath = findMainPath(level, collapsedGraph)
    val roomIdMap = mapRoomsToId(level)

    // Special case, no responders can be placed since no edges are available
    if (initialMainPath.vertices.size == 1) {
      return Some(markMainPath(level, TreePath(level, collapsedGraph, initialMainPath)))
    }

    var areaData = setupARData(level, new PointlessAreaData())
    val trimmed = edgeTrimmer.removeVertices(collapsedGraph, initialMainPath.vertices.toSet, collapsedNodeEval(areaData))
    val pathfinder = new LongestTreePath[CollapsedNode, CollapsedEdge]()
    val edgeSelection = new EdgeSelection()
    val edgeFinder = new ClosestEdgeFinder()

    /* Main algorithm */
    var responderLeftToAdd = level.responderAmount
    var updatedLevel = level
    var mainPath = MainPath(trimmed, findNode(level.start, trimmed), findNode(level.goal, trimmed))

    while (responderLeftToAdd > 0 && mainPath.pointlessAreas.nonEmpty) {
      val nextAreaCandidate = mainPath.pointlessAreas.head
      val pathToAdd = pathfinder.computeLongestPath(nextAreaCandidate.topology, nextAreaCandidate.mainPathConnection)
      val roomsToAddActivatorTo = pathToAdd.stop
      val edgesToAddResponderTo = edgeSelection.findEdgeCandidates(mainPath.currentPath.vertices, mainPath.currentPath.edges, nextAreaCandidate.mainPathConnection)
      val closestResult: Option[(CollapsedEdge[CollapsedNode], PointlessAreaData)] = edgeFinder.findEdge(edgesToAddResponderTo, areaData)

      if (closestResult.isDefined) {
        val edge = closestResult.get._1
        val originalTargetsOfEdge = CollapsedEdge.targetsOfEdge(edge)
        val g = level.asGraph // this has to be specified, or the compiler chokes
        val from = roomIdMap(originalTargetsOfEdge._1)
        val to = roomIdMap(originalTargetsOfEdge._2)
        val roomsInNode = removeCutpoints(roomsToAddActivatorTo, nextAreaCandidate.topology).map(roomIdMap)
        val regularEdge = g.get(from).edges.toVector.find(e => e.contains(to)).get

        // Update data
        updatedLevel = updatedLevel.addActivatorAndResponder(roomsInNode, regularEdge.toOuter)
        areaData = closestResult.get._2
        mainPath = mainPath.appendPath(pathToAdd.stop, nextAreaCandidate)
        responderLeftToAdd -= 1

      } else {

        // Activator room found, but no valid edge to place responders on exists
        mainPath = mainPath.removeNode(pathToAdd.stop, nextAreaCandidate)

      }
    }

    /* Let the level know which path the player is expected to take in order to check off all activators. */
    updatedLevel = markMainPath(updatedLevel, TreePath(level, collapsedGraph, mainPath.currentPath))
    Some(updatedLevel)
  }

  /* Used as input to the edge trimmer. */
  private def collapsedNodeEval(data: PointlessAreaData)(n: CollapsedNode): Boolean = n.vertexCollection.exists(data.canHoldActivator)

  private def findMainPath(level: L, g: Graph[CollapsedNode, CollapsedEdge]): Path[CollapsedNode, CollapsedEdge] = {
    val start = findNode(level.start, g)
    val goal = findNode(level.goal, g)
    findCollapsedPath(start, goal, g)
  }

  private def findCollapsedPath(start: CollapsedNode,
                                goal: CollapsedNode,
                                graph: Graph[CollapsedNode, CollapsedEdge]): Path[CollapsedNode, CollapsedEdge] = {

    BFSPathfinder().computePath[CollapsedNode, CollapsedEdge](start, goal, graph).getOrElse {
      throw new Error("No path from start " + start + " to goal " + goal + " found.")
    }
  }

  private def findNode(representedVertex: R, graph: Graph[CollapsedNode, CollapsedEdge]): CollapsedNode = {
    graph.nodes.find(n => n.contains(representedVertex.rid)).getOrElse {
      throw new Error("The vertex " + representedVertex + " was not found in the collapsed graph " + graph)
    }
  }

  private def setupARData(level: L, data: PointlessAreaData): PointlessAreaData =  markActivatorNodes(level, markResponderEdges(level, data))

  private def markActivatorNodes(level: L, data: PointlessAreaData): PointlessAreaData = {
    var d = data
    for (r <- GraphCommons.outerVertices(level.asGraph) if level.remainingActivatorCapacity(r) > 0)
      d = d.setActivatorCapacity(r.rid)
    d
  }

  private def markResponderEdges(level: L, data: PointlessAreaData): PointlessAreaData = {
    var d = data
    for (e <- GraphCommons.outerEdges(level.asGraph))
      d = d.setResponderCapacity(e._1.rid, e._2.rid, level.remainingResponderCapacity(e))
    d
  }

  private def mapRoomsToId(level: L): Map[Int, R] = GraphCommons.outerVertices(level.asGraph).map(r => r.rid -> r).toMap

  private def markMainPath(level: L, mainPath: TreePath): L = {
    level.markMainPath(mainPath)
  }

  private def removeCutpoints(node: CollapsedNode, topology: Graph[CollapsedNode, CollapsedEdge]): Set[Int] = {
    val cutpoints = GraphCommons.neighbors(node, topology).filter(_.isDummy).map(_.singleRepresentedVertex)
    node.vertexCollection -- cutpoints
  }
}
