package rldungeon.help.parameters

import net.cyndeline.scalarlib.rldungeon.dgs.ParameterEstimator
import rldungeon.help.{RoomVertex, GraphLevel, CorridorEdge}
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Checks how many rooms are in a graph.
 */
class RoomAmountEstimator extends ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge] {
  def value(level: GraphLevel): Double = level.asGraph.nodes.size
}
