package net.cyndeline.scalarlib.rldungeon.levelPath

import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.common.{IDCorridor, Room, Corridor}
import net.cyndeline.scalarlib.rlgraph.util.GraphConverter
import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Represents a biconnected component in a levels topology or a cutpoint joining multiple components.
 */
class TreeNode private (room: Option[Graph[Int, IDCorridor]], crdr: Option[Int], cp: Option[Int]) {
  require(room.isDefined || crdr.isDefined || cp.isDefined, "TreeNode requires one type of data to be defined.")
  require(Seq(room, crdr, cp).count(_.isDefined) == 1, "Cannot add more than one data type to TreeNode.")

  /** @return True if this node represents a level topology containing either a single room or a collection of
    *         rooms joined using two or more edges.
    */
  def isRoom = room.isDefined

  /** @return True if this node represents a single edge forming a biconnected component in the level (i.e there is
    *         only one path from one of its rooms to the other, and it is through this edge).
   */
  def isCorridor = crdr.isDefined

  /** @return True if the single room represented by this node is a member of multiple biconnected components with
    *         two or more edges.
    */
  def isCutPoint = cp.isDefined

  /** @return The room ID of the of the cutpoint this node represents. */
  def cutPoint: Int = {
    require(isCutPoint, "Cannot retrieve the cut point for a tree node of type " + nType)
    cp.get
  }

  /** @return A graph structure where every vertex is the room ID of the room they represent, and every edge contains
    *         the ID of the corridor that joins its two rooms together in the original level graph.
    */
  def roomStructure: Graph[Int, IDCorridor] = {
    require(isRoom, "Cannot retrieve the room structure of a tree node of type " + nType)
    room.get
  }

  /** @return the ID of the corridor represented by this node. */
  def corridor: Int = {
    require(isCorridor, "Cannot retrieve the corridor structure of a tree node of type " + nType)
    crdr.get
  }

  override def equals(other: Any): Boolean = other match {
    case tn: TreeNode => {
      if (tn.isCorridor == isCorridor && tn.isCutPoint == isCutPoint && tn.isRoom == isRoom) {

        if (isRoom)
          roomStructure == tn.roomStructure
        else if (isCorridor)
          corridor == tn.corridor
        else
          cutPoint == tn.cutPoint

      } else {
        false
      }
    }
    case _ => false
  }

  override def hashCode: Int = room.## ^ crdr.## ^ cp.##

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "TNode(" + nType + ")-("
    if (isRoom)
      builder ++= roomStructure.toString
    else if (isCorridor)
      builder ++= corridor.toString
    else if (isCutPoint)
      builder ++= cutPoint.toString

    builder ++= ")"
    builder.toString()
  }

  private def nType: String = if (isRoom) "Room" else if (isCorridor) "Corridor" else "Cut Point"
}

object TreeNode {

  /**
   * @param rooms The topology of a biconnected section of a level with 2 or more edges.
   * @tparam R Room type used in the level.
   * @tparam C Corridor type used in the level.
   * @return A node representing the topology.
   */
  def roomNode[R <: Room : TypeTag, C[X] <: UnDiEdge[X] with Corridor](rooms: Graph[R, C]) = {
    val tt = typeTag[IDCorridor[Int]]
    val converter = GraphConverter[R, Int, C, IDCorridor]((r: R) => r.rid, (e: C[R], a: Int, b: Int) => IDCorridor(a, b, e.cid))
    new TreeNode(Some(converter.convert(rooms)), None, None)
  }

  /**
   * @param c A minimal biconnected component with a single edge.
   * @tparam R Room type used in the level.
   * @tparam C Corridor type used in the level.
   * @return A node representing the edge.
   */
  def corridorNode[R <: Room, C[X] <: UnDiEdge[X] with Corridor](c: C[R]) = new TreeNode(None, Some(c.cid), None)

  /**
   * @param cp A single cutpoint in the level.
   * @tparam R Room type used in the level.
   * @return A node representing the cutpoint.
   */
  def cutPoint[R <: Room](cp: R) = new TreeNode(None, None, Some(cp.rid))

}
