package net.cyndeline.scalarlib.rldungeon.dgs

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Used by parameters to parse a level into a value representing the aspect of the level that the parameter is
 * interested in. Example: A parameter that is interested in the number of rooms in the level has a ParameterEstimator
 * that counts the number of rooms and returns the amount.
 */
trait ParameterEstimator[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] {

  /**
   * Parse a level into a numerical value.
   * @param level Level to parse.
   * @return a value representing some aspect of the graph.
   */
  def value(level: L): Double

}
