package net.cyndeline.scalarlib.rldungeon.dgs

import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}

import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Used by parameters to parse a level into a value representing the aspect of the level that the parameter is
 * interested in. Example: A parameter that is interested in the number of rooms in the level has a ParameterEstimator
 * that counts the number of rooms and returns the amount.
 */
trait ParameterEstimator[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X]] {

  /**
   * Parse a level into a numerical value.
   * @param level Level to parse.
   * @return a value representing some aspect of the graph.
   */
  def value(level: L): Double

}
