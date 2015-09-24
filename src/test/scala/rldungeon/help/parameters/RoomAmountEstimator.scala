package rldungeon.help.parameters

import net.cyndeline.scalarlib.rldungeon.dgs.ParameterEstimator
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}

/**
 * Checks how many rooms are in a graph.
 */
class RoomAmountEstimator extends ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge] {
  def value(level: GraphLevel): Double = level.asGraph.nodes.size
}
