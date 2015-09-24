package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedNode, CollapsedEdge}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.PointlessAreaData

/**
 * Examines a sequence of edges along the main path that are candidates for receiving a responder, and selects the
 * one closest to the beginning of each sequence in order to maximize the probability of selecting an edge that lies
 * close to the node that received the responder.
 */
class ClosestEdgeFinder {

  /**
   * @param edges Edge candidate sequences.
   * @param data Used to check if an edge can carry additional responders.
   * @return The edge candidate if one is found, and the updated data object.
   */
  def findEdge(edges: Vector[Vector[CollapsedEdge[CollapsedNode]]],
               data: PointlessAreaData): Option[(CollapsedEdge[CollapsedNode], PointlessAreaData)] = {
    var closestPositionFound = Int.MaxValue
    val allSequences = edges.iterator
    var currentEdgeCandidate: Option[CollapsedEdge[CollapsedNode]] = None

    while (allSequences.hasNext) {
      val seq = allSequences.next()

      var i = 0
      val edgesInSeq = seq.iterator
      while (edgesInSeq.hasNext && i < closestPositionFound) {
        val edge = edgesInSeq.next()

        if (!edge.isDummy) {
          val targets = CollapsedEdge.targetsOfEdge(edge)

          if (data.canHoldResponder(targets._1, targets._2)) {
            currentEdgeCandidate = Some(edge)
            closestPositionFound = i
          }

        }
        i += 1
      }
    }

    if (currentEdgeCandidate.isDefined) {
      val targets = CollapsedEdge.targetsOfEdge(currentEdgeCandidate.get)
      Some((currentEdgeCandidate.get, data.reduceResponderCapacity(targets._1, targets._2)))
    } else {
      None
    }
  }

}
