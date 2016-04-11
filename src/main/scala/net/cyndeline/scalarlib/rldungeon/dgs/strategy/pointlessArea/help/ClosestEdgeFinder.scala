package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData

/**
 * Examines a sequence of edges along the main path that are candidates for receiving a responder, and selects the
 * one closest to the main area connection of the pointless area receiving the activator. Also reduces the responder
  * capacity of the edge.
 */
class ClosestEdgeFinder {

  /**
   * @param edges Edge candidates, where an edge appears closer to the beginning og the vector if it appears closer to
    *             the main area connection of a pointless area.
   * @param data Used to check if an edge can carry additional responders.
   * @return The edge candidate if one is found, and the updated data object.
   */
  def findEdge(edges: Vector[CollapsedEdge[CollapsedNode]],
               data: PointlessAreaData): Option[(CollapsedEdge[CollapsedNode], PointlessAreaData)] = {

    for (edge <- edges if !edge.isDummy) {
      val targets = CollapsedEdge.targetsOfEdge(edge)

      if (data.canHoldResponder(targets._1, targets._2)) {
        return Some((edge, data.reduceResponderCapacity(targets._1, targets._2)))
      }
    }

    None
  }

}
