package net.cyndeline.scalarlib.rldungeon.grammar

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef.OuterEdge
import net.cyndeline.scalarlib.rldungeon.common.{Room, Level}

/**
 * Decides how a set of productions should be applied to construct a graph representing a game map. This includes
 * not only topology, but also game content.
 *
 * @tparam R Room type in the level that the strategy should be applied to.
 * @tparam C Corridor/connection type in the level that the strategy should be applied to.
 */
trait Strategy[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]] {

  /**
   * Modifies a level and outputs the result.
   *
   * @param level The level that the productions initially will be applied to.
   * @return The finished level if one was possible to produce according to the strategy, otherwise None.
   *         The resulting level must have a single start room and a single goal, and be connected.
   */
  def apply(level: L): Option[L]

}
