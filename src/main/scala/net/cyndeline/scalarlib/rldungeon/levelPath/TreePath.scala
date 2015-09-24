package net.cyndeline.scalarlib.rldungeon.levelPath

import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.common.{Corridor, Level, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

/**
 * This class represents levels by folding biconnected components of the levels graph structure into super-nodes
 * (representing every room inside the component), thus reducing the levels structure to a tree. The reason for this
 * is that examining levels may take a long time due to extensive path-finding being necessary. By representing the
 * level without cycles, any given pair of (super) nodes will only have a single path between them, making it easier to
 * traverse the level.
 *
 * Nodes in the TreePath represents one of two things: Either a biconnected component with multiple edges (a subsection
 * of the level where every room has at least two paths to every other room) or a biconnected component with a single
 * edge (two rooms where the only path between them goes via a single corridor). Every node is connected using a
 * Branch. Branches do not take the place of corridors in the level (they're all in the TreeNodes), they only serve
 * to show which components allow travel between each other, and which common room the player traverses when going
 * from one component to another.
 *
 * One special case to this is the cutpoint: A room that is a member of two different biconnected components with
 * multiple edges. When this occurs, the cutpoint will be separated into its own TreeNode. The room represented by it
 * will however still be present in each components TreeNode that the point is connected to. When simulating level
 * traversals, rooms present in TreeNodes representing cutpoints will be evaluated multiple times unless the user
 * takes care not to process duplicates. The reason for storing cutpoints in both their own separate TreeNodes as well
 * as in the components they're a member of, is to more accurately preserve the levels underlying topology. If levels
 * where three or more components shared a single cutpoint were allowed to connect to each other with branches however,
 * the level would loop at these locations.
 *
 * The TreePath contains two elements: The main path through the level, and the pointless areas (areas that aren't
 * visited when passing through the man path). Unlike the level, both of these use directed edges to show the direction
 * the player will be going. In the case of the main path, this is to make it easier to display how rooms are
 * traversed if the path loops. In pointless areas, the room connection on the main path will not have any ingoing
 * branches. From there, edges will be directed outwards towards the outermost leaf components in each area
 * (TreeNodes that lacks outgoing edges).
 *
 * @param mainPath The sections of the level, in the order they must be traversed when completing the level.
 * @param pointlessAreas Every section of the level not covered by the main path. Each sections contains rooms that can
 *                       be reached from one another without passing the main area. The node with no incoming edges are
 *                       the section on the main area that the pointless area starts at. Nodes with no outgoing edges
 *                       are the outermost sections of the area that area dead ends.
 */
class TreePath private (val mainPath: Path[TreeNode, Branch], val pointlessAreas: Vector[Graph[TreeNode, Branch]]) {

  override def equals(other: Any): Boolean = other match {
    case tp: TreePath => mainPath == tp.mainPath && pointlessAreas == tp.pointlessAreas
    case _ => false
  }

  override def hashCode: Int = mainPath.## ^ pointlessAreas.##

  override def toString: String = {
    val builder = new StringBuilder()
    val nl = System.getProperty("line.separator")
    builder ++= "-- Tree path --" + nl
    builder ++= "Main path: " + mainPath
    if (!pointlessAreas.isEmpty) {
      builder ++= "Pointless areas:"
      for (pa <- pointlessAreas)
        builder ++= " Area >> " + pa.toString
    }
    builder.toString()
  }

}

/**
 * Companion object used to construct TreePaths.
 */
object TreePath {

  /**
   * @param level Level that the TreePath should represent.
   * @param totalGraph The collapsed super-graph of the level.
   * @param mainPath The final main path through the level.
   * @tparam L Level type.
   * @tparam R Room type.
   * @tparam C Corridor type.
   * @return A tree path with the sections of the level as they are traversed along the main path, and the pointless
   *         areas oriented from their main path connection to their outermost subsections.
   */
  def apply[L <: Level[L, R, C], R <: Room : TypeTag, C[X] <: UnDiEdge[X] with Corridor: ({type l[M[_]] = TypeTag[M[R]]})#l]
    (level: Level[L, R, C],
     totalGraph: Graph[CollapsedNode, CollapsedEdge],
     mainPath: Path[CollapsedNode, CollapsedEdge]): TreePath = {

    val idToRoom: Map[Int, R] = GraphCommons.outerVertices(level.asGraph).map(r => r.rid -> r).toMap

    /* Maps each collapsed node to the graph structure containing its underlying topology. */
    val cNodeToGraph = createTreeNodesForRooms[R, C](level.asGraph, totalGraph, idToRoom) ++ createTreeNodesForDummies[R, C](totalGraph, idToRoom)
    val cEdgeToNde = createTreeNodesForCorridors(level.asGraph, totalGraph, idToRoom)

    val mainTreePath = createMainPath(mainPath, cNodeToGraph, cEdgeToNde, idToRoom)
    val pointless = computePointlessAreas(totalGraph, mainPath, cNodeToGraph, cEdgeToNde, idToRoom)

    new TreePath(mainTreePath, pointless)
  }

  private def createMainPath[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (mainPath: Path[CollapsedNode, CollapsedEdge],
      cnToTN: Map[CollapsedNode, TreeNode],
      ceToTN: Map[CollapsedEdge[CollapsedNode], TreeNode],
      rooms: Map[Int, R]): Path[TreeNode, Branch] = {
    val branches = new ListBuffer[Branch[TreeNode]]()
    var previousNode = mainPath.vertices.head

    for (e <- mainPath.edges) {
      if (!e.isDummy) {
        val firstIsPreviousNode = e._1 == previousNode
        val next = if (firstIsPreviousNode) e._2 else e._1
        branches ++= collapsedEdgeToBranch(e, previousNode, cnToTN, ceToTN)
        previousNode = next

      } else {
        val next = if (previousNode.isDummy) e.find(!_.isDummy).get else e.find(_.isDummy).get
        branches ++= collapsedEdgeToBranch(e, previousNode, cnToTN, ceToTN)
        previousNode = next

      }
    }

    if (branches.isEmpty)
      Path[TreeNode, Branch](cnToTN(mainPath.start))
    else
      Path(cnToTN(mainPath.start), branches.toVector)
  }

  private def computePointlessAreas[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (totalGraph: Graph[CollapsedNode, CollapsedEdge],
      mainPath: Path[CollapsedNode, CollapsedEdge],
      cnToTN: Map[CollapsedNode, TreeNode],
      ceToTN: Map[CollapsedEdge[CollapsedNode], TreeNode],
      rooms: Map[Int, R]): Vector[Graph[TreeNode, Branch]] = {
    val areas = new ListBuffer[Graph[TreeNode, Branch]]()
    val edgeSet = mainPath.edges.toSet
    val graphWithoutMainPathEdges = totalGraph.filterNot(totalGraph.having(edge = e => edgeSet.contains(e.toOuter)))

    for (point <- mainPath.vertices if graphWithoutMainPathEdges.get(point).degree > 0; neighbor <- GraphCommons.outerEdgeNeighbors(point, graphWithoutMainPathEdges)) {
      val edge = neighbor._2
      val subGraph = GraphCommons.computeSubTopology[CollapsedNode, CollapsedEdge](edge, point, graphWithoutMainPathEdges)
      areas += makePointlessAreaIntoDirectedTree(subGraph, point, cnToTN, ceToTN)
    }

    areas.toVector
  }

  private def makePointlessAreaIntoDirectedTree[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (area: Graph[CollapsedNode, CollapsedEdge],
     root: CollapsedNode,
     cnToTN: Map[CollapsedNode, TreeNode],
     ceToTN: Map[CollapsedEdge[CollapsedNode], TreeNode]): Graph[TreeNode, Branch] = {
    val r = area.get(root)
    val allEdges = (for (e <- r.edges.toVector) yield directSubTree(area, cnToTN, ceToTN)(r, e)).flatten

    Graph.from(Nil, allEdges)
  }

  private def directSubTree[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (area: Graph[CollapsedNode, CollapsedEdge],
     cnToTN: Map[CollapsedNode, TreeNode],
     ceToTN: Map[CollapsedEdge[CollapsedNode], TreeNode])
    (previous: area.NodeT, edge: area.EdgeT): Vector[Branch[TreeNode]] = {
    val nextNode = edge.find(_ != previous).get
    val nextEdgeSet = nextNode.edges.filterNot(_.contains(previous))

    if (nextEdgeSet.isEmpty) {
      collapsedEdgeToBranch(edge.toOuter, previous, cnToTN, ceToTN)
    } else {
      var result = new ListBuffer[Branch[TreeNode]]()
      result ++= collapsedEdgeToBranch(edge.toOuter, previous, cnToTN, ceToTN)
      val it = nextEdgeSet.iterator
      while (it.hasNext) {
        val nextEdge = it.next()
        result ++= directSubTree(area, cnToTN, ceToTN)(nextNode, nextEdge)
      }

      result.toVector
    }
  }

  private def collapsedEdgeToBranch[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (e: CollapsedEdge[CollapsedNode],
     from: CollapsedNode,
     cnToTN: Map[CollapsedNode, TreeNode],
     ceToTN: Map[CollapsedEdge[CollapsedNode], TreeNode]): Vector[Branch[TreeNode]] = {

    if (!e.isDummy) {
      val targets = CollapsedEdge.targetsOfEdge(e)
      val firstIsPreviousNode = e._1 == from
      val previousTarget = if (firstIsPreviousNode) targets._1 else targets._2
      val nextTarget = if (firstIsPreviousNode) targets._2 else targets._1
      val next = if (firstIsPreviousNode) e._2 else e._1

      Vector(Branch(cnToTN(from), ceToTN(e), previousTarget), Branch(ceToTN(e), cnToTN(next), nextTarget))

    } else {
      val next = if (from.isDummy) e.find(!_.isDummy).get else e.find(_.isDummy).get
      val connection = if (from.isDummy) from.singleRepresentedVertex else next.singleRepresentedVertex
      Vector(Branch(cnToTN(from), cnToTN(next), connection))
    }
  }

  /* Adds multi-edge components to tree nodes. Also finds the cutpoints missing in multi-edge components and adds
   * them back in.
   */
  private def createTreeNodesForRooms[R <: Room : TypeTag, C[X] <: UnDiEdge[X] with Corridor: ({type l[M[_]] = TypeTag[M[R]]})#l]
    (originalGraph: Graph[R, C],
    collapsedGraph: Graph[CollapsedNode, CollapsedEdge],
    rooms: Map[Int, R]): Map[CollapsedNode, TreeNode] = {

    val map = new mutable.HashMap[CollapsedNode, TreeNode]()

    for (n <- collapsedGraph.nodes if !n.isDummy) {
      val connectedDummyEdges = n.edges.filter(_.isDummy)
      val cutpoints = connectedDummyEdges.map(e => e.find(inner => inner != n).get).map(_.singleRepresentedVertex)
      val allVertices: Set[Int] = n.vertexCollection ++ cutpoints
      val subGraph = GraphCommons.traversedSubGraph[R, C](originalGraph, rooms(allVertices.head), (e: C[R]) => allVertices.contains(e._1.rid) && allVertices.contains(e._2.rid))
      val outer: CollapsedNode = n
      map += (outer -> TreeNode.roomNode(subGraph))
    }

    map.toMap
  }

  /* Adds single-edge components to tree nodes. Dummy edges not included. */
  private def createTreeNodesForCorridors[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (originalGraph: Graph[R, C],
    collapsedGraph: Graph[CollapsedNode, CollapsedEdge],
    rooms: Map[Int, R]): Map[CollapsedEdge[CollapsedNode], TreeNode] = {

    val map = new mutable.HashMap[CollapsedEdge[CollapsedNode], TreeNode]()

    for (e <- collapsedGraph.edges if !e.isDummy) {
      val targets = CollapsedEdge.targetsOfEdge(e.toOuter)
      val originalEdge = originalGraph.get(rooms(targets._1)).findOutgoingTo(originalGraph.get(rooms(targets._2))).get
      map += (e.toOuter -> TreeNode.corridorNode(originalEdge.toOuter))
    }

    map.toMap
  }

  private def createTreeNodesForDummies[R <: Room, C[X] <: UnDiEdge[X] with Corridor]
    (collapsedGraph: Graph[CollapsedNode, CollapsedEdge],
     rooms: Map[Int, R]): Map[CollapsedNode, TreeNode] = {
    val map = new mutable.HashMap[CollapsedNode, TreeNode]()

    for (n <- collapsedGraph.nodes if n.isDummy) {
      val outer: CollapsedNode = n
      map += (outer -> TreeNode.cutPoint[R](rooms(n.singleRepresentedVertex)))
    }

    map.toMap
  }

}